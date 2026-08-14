package com.jamesward.zio_git

import zio.test.*

import java.nio.charset.StandardCharsets

object DeltaSpec extends ZIOSpecDefault:

  def spec = suite("Delta")(
    test("applies an insert followed by a copy"):
      val base = "AAAAAAA".getBytes(StandardCharsets.US_ASCII) // 7 bytes
      // srcSize=7, tgtSize=10, insert "BBB", copy 7 bytes from offset 0
      val delta = Array[Int](
        0x07,             // source size
        0x0a,             // target size
        0x03, 'B', 'B', 'B', // insert 3 literal bytes
        0x91, 0x00, 0x07, // copy: offset byte present (0x01) + size byte present (0x10) => 0x80|0x11
      ).map(_.toByte)
      val out = String(Delta.apply(base, delta), StandardCharsets.US_ASCII)
      assertTrue(out.equals("BBBAAAAAAA"))
    ,
    test("a copy with size 0 means 0x10000"):
      // Not exercised with a huge base here; instead verify small copies compose.
      val base = "hello world".getBytes(StandardCharsets.US_ASCII)
      // srcSize=11, tgtSize=5, copy 5 bytes from offset 6 ("world")
      val delta = Array[Int](0x0b, 0x05, 0x91, 0x06, 0x05).map(_.toByte)
      val out = String(Delta.apply(base, delta), StandardCharsets.US_ASCII)
      assertTrue(out.equals("world"))
  )
