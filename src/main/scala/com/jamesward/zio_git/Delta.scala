package com.jamesward.zio_git

import java.io.ByteArrayOutputStream

/**
 * Applies a git delta (as found in `OFS_DELTA` / `REF_DELTA` pack entries) to a
 * base object's bytes. A delta is a source-size varint, a target-size varint,
 * then a sequence of copy/insert instructions.
 *
 * See: https://git-scm.com/docs/pack-format#_deltified_representation
 */
object Delta:

  def apply(base: Array[Byte], delta: Array[Byte]): Array[Byte] =
    val (_, afterSrc) = readSize(delta, 0)
    val (targetSize, afterTarget) = readSize(delta, afterSrc)
    val out = ByteArrayOutputStream(targetSize)
    var i = afterTarget
    while i < delta.length do
      val op = delta(i) & 0xff
      i += 1
      if (op & 0x80) != 0 then
        // Copy `size` bytes from `offset` in the base.
        var copyOffset = 0
        var copySize = 0
        if (op & 0x01) != 0 then { copyOffset |= (delta(i) & 0xff);        i += 1 }
        if (op & 0x02) != 0 then { copyOffset |= (delta(i) & 0xff) << 8;   i += 1 }
        if (op & 0x04) != 0 then { copyOffset |= (delta(i) & 0xff) << 16;  i += 1 }
        if (op & 0x08) != 0 then { copyOffset |= (delta(i) & 0xff) << 24;  i += 1 }
        if (op & 0x10) != 0 then { copySize |= (delta(i) & 0xff);          i += 1 }
        if (op & 0x20) != 0 then { copySize |= (delta(i) & 0xff) << 8;     i += 1 }
        if (op & 0x40) != 0 then { copySize |= (delta(i) & 0xff) << 16;    i += 1 }
        val size = if copySize == 0 then 0x10000 else copySize
        out.write(base, copyOffset, size)
      else if op != 0 then
        // Insert the next `op` literal bytes from the delta.
        out.write(delta, i, op)
        i += op
      else
        throw IllegalArgumentException("delta opcode 0x00 is reserved")
    out.toByteArray

  // Little-endian base-128 varint (7 data bits per byte, MSB = continuation).
  private def readSize(delta: Array[Byte], offset: Int): (Int, Int) =
    var pos = offset
    var size = 0
    var shift = 0
    var b = 0
    while
      b = delta(pos) & 0xff
      pos += 1
      size |= (b & 0x7f) << shift
      shift += 7
      (b & 0x80) != 0
    do ()
    (size, pos)
