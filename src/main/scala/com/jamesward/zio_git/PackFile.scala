package com.jamesward.zio_git

import zio.Chunk

import java.util.zip.{DataFormatException, Inflater}
import scala.collection.mutable

/**
 * Parser for a git packfile (the body of a `git-upload-pack` response, once the
 * pkt-line negotiation prefix is stripped). Handles the v2 header, the
 * per-object type/size varint header, per-object zlib inflation, and both
 * `OFS_DELTA` and `REF_DELTA` resolution against a self-contained (non-thin)
 * pack.
 *
 * See: https://git-scm.com/docs/pack-format
 */
object PackFile:

  private val ObjCommit = 1
  private val ObjTree = 2
  private val ObjBlob = 3
  private val ObjTag = 4
  private val ObjOfsDelta = 6
  private val ObjRefDelta = 7

  private enum RawEntry:
    case Whole(objType: GitObjectType, data: Array[Byte])
    case OfsDelta(baseOffset: Int, delta: Array[Byte])
    case RefDelta(baseId: ObjectId, delta: Array[Byte])

  /**
   * Parse a packfile into a map of object id to resolved object.
   *
   * The body reads a binary format and resolves deltas that can forward- and
   * backward-reference other entries, so it is written imperatively with
   * locally-scoped mutable maps (a work-list with memoization). All mutation is
   * contained here; the result is an immutable map. Malformed input surfaces as
   * a `Left(message)`.
   */
  def parse(bytes: Array[Byte]): Either[String, Map[ObjectId, RawObject]] =
    try parseUnsafe(bytes)
    catch
      case e: DataFormatException      => Left(s"zlib inflation failed: ${e.getMessage}")
      case e: IndexOutOfBoundsException => Left(s"packfile truncated: ${e.getMessage}")

  private def parseUnsafe(bytes: Array[Byte]): Either[String, Map[ObjectId, RawObject]] =
    if bytes.length < 12 then Left("packfile too short for header")
    else if !(bytes(0) == 'P' && bytes(1) == 'A' && bytes(2) == 'C' && bytes(3) == 'K') then
      Left("missing PACK signature")
    else
      val version = readUInt32(bytes, 4)
      if version != 2L then Left(s"unsupported pack version: $version")
      else
        val count = readUInt32(bytes, 8).toInt
        readEntries(bytes, count).flatMap(resolve)

  private def readEntries(bytes: Array[Byte], count: Int): Either[String, Vector[(Int, RawEntry)]] =
    val entries = Vector.newBuilder[(Int, RawEntry)]
    var offset = 12
    var i = 0
    var error: Option[String] = None
    while i < count && error.isEmpty do
      val entryOffset = offset
      val (typeCode, size, afterHeader) = readObjectHeader(bytes, offset)
      typeCode match
        case ObjCommit | ObjTree | ObjBlob | ObjTag =>
          val (data, consumed) = inflate(bytes, afterHeader, size)
          GitObjectType.fromPackCode(typeCode) match
            case Some(objType) =>
              entries += (entryOffset -> RawEntry.Whole(objType, data))
              offset = afterHeader + consumed
            case None =>
              error = Some(s"unexpected object type code: $typeCode")
        case ObjOfsDelta =>
          val (baseRel, afterBase) = readOffsetVarint(bytes, afterHeader)
          val (delta, consumed) = inflate(bytes, afterBase, size)
          entries += (entryOffset -> RawEntry.OfsDelta(entryOffset - baseRel, delta))
          offset = afterBase + consumed
        case ObjRefDelta =>
          val baseBytes = bytes.slice(afterHeader, afterHeader + 20)
          ObjectId.fromRawBytes(baseBytes) match
            case Some(baseId) =>
              val (delta, consumed) = inflate(bytes, afterHeader + 20, size)
              entries += (entryOffset -> RawEntry.RefDelta(baseId, delta))
              offset = afterHeader + 20 + consumed
            case None =>
              error = Some("ref-delta base id was not 20 bytes")
        case other =>
          error = Some(s"unknown pack object type code: $other")
      i += 1
    error.toLeft(entries.result())

  private def resolve(entries: Vector[(Int, RawEntry)]): Either[String, Map[ObjectId, RawObject]] =
    val byOffset: Map[Int, RawEntry] = entries.toMap
    val resolvedByOffset = mutable.HashMap.empty[Int, RawObject]
    val offsetById = mutable.HashMap.empty[ObjectId, Int]

    def store(offset: Int, obj: RawObject): RawObject =
      resolvedByOffset.update(offset, obj)
      offsetById.update(obj.id, offset)
      obj

    // Resolve everything reachable purely by (always-backward) offset refs.
    def resolveByOffset(offset: Int): RawObject =
      resolvedByOffset.get(offset) match
        case Some(obj) => obj
        case None =>
          byOffset(offset) match
            case RawEntry.Whole(objType, data) =>
              store(offset, RawObject(objType, Chunk.fromArray(data)))
            case RawEntry.OfsDelta(baseOffset, delta) =>
              val base = resolveByOffset(baseOffset)
              val applied = Delta.apply(base.data.toArray, delta)
              store(offset, RawObject(base.objType, Chunk.fromArray(applied)))
            case _: RawEntry.RefDelta =>
              // Deferred to the ref-delta fixpoint below.
              null

    entries.foreach: (offset, entry) =>
      entry match
        case _: RawEntry.RefDelta => ()
        case _                    => resolveByOffset(offset)

    // Ref-deltas reference their base by id, which may be another (as-yet
    // unresolved) ref-delta, so resolve them by fixpoint until none remain.
    var pending = entries.collect { case (offset, rd: RawEntry.RefDelta) => offset -> rd }
    var progressed = true
    var failure: Option[String] = None
    while pending.nonEmpty && progressed && failure.isEmpty do
      progressed = false
      val stillPending = Vector.newBuilder[(Int, RawEntry.RefDelta)]
      pending.foreach: (offset, rd) =>
        offsetById.get(rd.baseId) match
          case Some(baseOffset) =>
            val base = resolvedByOffset(baseOffset)
            val applied = Delta.apply(base.data.toArray, rd.delta)
            store(offset, RawObject(base.objType, Chunk.fromArray(applied)))
            progressed = true
          case None =>
            stillPending += (offset -> rd)
      pending = stillPending.result()

    if pending.nonEmpty then
      Left(s"unresolved ref-delta base(s); thin packs are not supported (${pending.size} left)")
    else
      Right(resolvedByOffset.map((_, obj) => obj.id -> obj).toMap)

  // Type (3 bits) + size (variable). The size's low 4 bits are in the first
  // byte; each continuation byte adds 7 more bits, little-endian.
  private def readObjectHeader(bytes: Array[Byte], offset: Int): (Int, Int, Int) =
    var pos = offset
    val first = bytes(pos) & 0xff
    pos += 1
    val typeCode = (first >> 4) & 0x7
    var size = first & 0x0f
    var shift = 4
    var b = first
    while (b & 0x80) != 0 do
      b = bytes(pos) & 0xff
      pos += 1
      size |= (b & 0x7f) << shift
      shift += 7
    (typeCode, size, pos)

  // OFS_DELTA base-offset encoding (a self-delimiting big-endian varint with an
  // implicit +1 carry between continuation bytes).
  private def readOffsetVarint(bytes: Array[Byte], offset: Int): (Int, Int) =
    var pos = offset
    var b = bytes(pos) & 0xff
    pos += 1
    var value = b & 0x7f
    while (b & 0x80) != 0 do
      b = bytes(pos) & 0xff
      pos += 1
      value = ((value + 1) << 7) | (b & 0x7f)
    (value, pos)

  // Inflate exactly `expectedSize` bytes of the object's content starting at
  // `offset`; returns the content and the number of compressed bytes consumed.
  // We must drive the inflater all the way to `finished()` (not just until
  // `expectedSize` output bytes are produced) so `getTotalIn` counts the whole
  // compressed stream INCLUDING its zlib trailer — otherwise the next object's
  // offset would be undercounted and the parse would drift into garbage.
  private def inflate(bytes: Array[Byte], offset: Int, expectedSize: Int): (Array[Byte], Int) =
    val inflater = Inflater()
    inflater.setInput(bytes, offset, bytes.length - offset)
    val out = new Array[Byte](expectedSize)
    val scratch = new Array[Byte](1) // absorbs the trailing 0-output finish call
    var produced = 0
    while !inflater.finished() do
      val n =
        if produced < expectedSize then inflater.inflate(out, produced, expectedSize - produced)
        else inflater.inflate(scratch)
      if n == 0 && !inflater.finished() && (inflater.needsInput() || inflater.needsDictionary()) then
        inflater.end()
        throw DataFormatException(s"incomplete deflate stream at offset $offset")
      produced += (if produced < expectedSize then n else 0)
    val consumed = inflater.getTotalIn
    inflater.end()
    (out, consumed)

  private def readUInt32(bytes: Array[Byte], offset: Int): Long =
    ((bytes(offset) & 0xffL) << 24) |
      ((bytes(offset + 1) & 0xffL) << 16) |
      ((bytes(offset + 2) & 0xffL) << 8) |
      (bytes(offset + 3) & 0xffL)
