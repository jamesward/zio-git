package com.jamesward.zio_git

import zio.Chunk
import zio.test.*

import java.nio.charset.StandardCharsets
import java.util.zip.Deflater

object PackFileSpec extends ZIOSpecDefault:

  private def deflate(data: Array[Byte]): Array[Byte] =
    val deflater = Deflater()
    deflater.setInput(data)
    deflater.finish()
    val buffer = new Array[Byte](data.length + 64)
    val n = deflater.deflate(buffer)
    deflater.end()
    buffer.slice(0, n)

  private def be32(n: Int): Array[Byte] =
    Array((n >> 24) & 0xff, (n >> 16) & 0xff, (n >> 8) & 0xff, n & 0xff).map(_.toByte)

  private def packHeader(count: Int): Array[Byte] =
    "PACK".getBytes(StandardCharsets.US_ASCII) ++ be32(2) ++ be32(count)

  // Encode a pack object header: 3 type bits + a little-endian base-128 size
  // (low 4 bits in the first byte, then 7 bits per continuation byte).
  private def objHeader(typeCode: Int, size: Int): Array[Byte] =
    val out = scala.collection.mutable.ArrayBuffer[Int]()
    var first = (typeCode << 4) | (size & 0x0f)
    var s = size >> 4
    if s > 0 then first |= 0x80
    out += first
    while s > 0 do
      var b = s & 0x7f
      s >>= 7
      if s > 0 then b |= 0x80
      out += b
    out.map(_.toByte).toArray

  private def wholeEntry(typeCode: Int, content: Array[Byte]): Array[Byte] =
    objHeader(typeCode, content.length) ++ deflate(content)

  // Reverse of PackFile.readOffsetVarint (the git OFS_DELTA base-offset varint
  // with the implicit +1 carry between continuation bytes).
  private def encodeOfs(n: Int): Array[Byte] =
    val tmp = scala.collection.mutable.ListBuffer[Int]()
    tmp.prepend(n & 0x7f)
    var value = (n >> 7) - 1
    while value >= 0 do
      tmp.prepend(0x80 | (value & 0x7f))
      value = (value >> 7) - 1
    tmp.map(_.toByte).toArray

  private def hexToBytes(hex: String): Array[Byte] =
    hex.grouped(2).map(h => Integer.parseInt(h, 16).toByte).toArray

  // A minimal insert-only delta: source size, target size, one insert op, then
  // the literal target bytes (so it reproduces `target` regardless of base).
  private def insertDelta(srcLen: Int, target: Array[Byte]): Array[Byte] =
    Array((srcLen & 0x7f).toByte, (target.length & 0x7f).toByte, (target.length & 0x7f).toByte) ++ target

  private def blobId(content: Array[Byte]): ObjectId =
    RawObject(GitObjectType.Blob, Chunk.fromArray(content)).id

  private def contents(objects: Map[ObjectId, RawObject]): Set[String] =
    objects.values.map(o => String(o.data.toArray, StandardCharsets.US_ASCII)).toSet

  // A minimal v2 pack containing one whole blob object.
  private def singleBlobPack(content: Array[Byte]): Array[Byte] =
    packHeader(1) ++ wholeEntry(3, content)

  def spec = suite("PackFile")(
    test("parses a single-blob pack and computes the git object id"):
      val content = "hello\n".getBytes(StandardCharsets.US_ASCII)
      val result = PackFile.parse(singleBlobPack(content))
      val objects = result.toOption.getOrElse(Map.empty)
      val ids = objects.keySet.map(_.hex)
      val blob = objects.values.headOption
      assertTrue(
        result.isRight,
        objects.size == 1,
        ids.contains("ce013625030ba8dba906f756967f9e9ca394464a"),
        blob.exists(_.objType == GitObjectType.Blob),
        blob.exists(o => String(o.data.toArray, StandardCharsets.US_ASCII).equals("hello\n")),
      )
    ,
    test("rejects bytes without a PACK signature"):
      assertTrue(PackFile.parse("nope".getBytes(StandardCharsets.US_ASCII)).isLeft)
    ,
    // Regression: two concatenated deflate streams. Requires each object's
    // compressed length (incl. the zlib trailer) to be counted so the second
    // object's offset is right — a single-object pack can't catch this.
    test("parses a pack with two whole objects (offsets must not drift)"):
      val b1 = "hello\n".getBytes(StandardCharsets.US_ASCII)
      val b2 = "world!!\n".getBytes(StandardCharsets.US_ASCII)
      val pack = packHeader(2) ++ wholeEntry(3, b1) ++ wholeEntry(3, b2)
      val result = PackFile.parse(pack)
      val objects = result.toOption.getOrElse(Map.empty)
      assertTrue(
        result.isRight,
        objects.size == 2,
        contents(objects) == Set("hello\n", "world!!\n"),
        objects.keySet == Set(blobId(b1), blobId(b2)),
      )
    ,
    // A >=16-byte object forces a multi-byte type/size header, and a following
    // object re-checks the post-inflate offset advance for large objects.
    test("parses a large object followed by another (multi-byte size header)"):
      val big = Array.fill(200)('x'.toByte)
      val small = "end\n".getBytes(StandardCharsets.US_ASCII)
      val pack = packHeader(2) ++ wholeEntry(3, big) ++ wholeEntry(3, small)
      val result = PackFile.parse(pack)
      val objects = result.toOption.getOrElse(Map.empty)
      assertTrue(
        result.isRight,
        objects.size == 2,
        objects.get(blobId(big)).exists(_.data.length == 200),
        objects.get(blobId(small)).exists(o => String(o.data.toArray, StandardCharsets.US_ASCII).equals("end\n")),
      )
    ,
    test("resolves a REF_DELTA against a base object earlier in the pack"):
      val base = "AAAAAAA".getBytes(StandardCharsets.US_ASCII)
      val target = "HELLO".getBytes(StandardCharsets.US_ASCII)
      val delta = insertDelta(base.length, target)
      val refDeltaEntry = objHeader(7, delta.length) ++ hexToBytes(blobId(base).hex) ++ deflate(delta)
      val pack = packHeader(2) ++ wholeEntry(3, base) ++ refDeltaEntry
      val result = PackFile.parse(pack)
      val objects = result.toOption.getOrElse(Map.empty)
      assertTrue(
        result.isRight,
        objects.size == 2,
        contents(objects) == Set("AAAAAAA", "HELLO"),
        objects.get(blobId(target)).exists(_.objType == GitObjectType.Blob),
      )
    ,
    test("resolves an OFS_DELTA against a base object earlier in the pack"):
      val base = "AAAAAAA".getBytes(StandardCharsets.US_ASCII)
      val target = "HELLO".getBytes(StandardCharsets.US_ASCII)
      val delta = insertDelta(base.length, target)
      val baseEntry = wholeEntry(3, base)
      // The OFS entry sits right after the base entry (which starts at offset
      // 12), so the back-reference distance is exactly the base entry's length.
      val ofsEntry = objHeader(6, delta.length) ++ encodeOfs(baseEntry.length) ++ deflate(delta)
      val pack = packHeader(2) ++ baseEntry ++ ofsEntry
      val result = PackFile.parse(pack)
      val objects = result.toOption.getOrElse(Map.empty)
      assertTrue(
        result.isRight,
        objects.size == 2,
        contents(objects) == Set("AAAAAAA", "HELLO"),
      )
  )
