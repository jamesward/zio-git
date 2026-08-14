package com.jamesward.zio_git

import zio.test.*

import java.nio.charset.StandardCharsets

object PktLineSpec extends ZIOSpecDefault:

  def spec = suite("PktLine")(
    test("encodes a line with a 4-hex length prefix that includes the prefix itself"):
      // "hi\n" (3 bytes) + 4 prefix bytes = 7 = 0x0007
      val encoded = PktLine.encodeLine("hi\n")
      val prefix = String(encoded.take(4), StandardCharsets.US_ASCII)
      assertTrue(
        prefix.equals("0007"),
        encoded.length == 7,
      )
    ,
    test("round-trips an encoded data frame"):
      val encoded = PktLine.encodeLine("want deadbeef\n")
      val decoded = PktLine.readFrame(encoded, 0)
      val payload = decoded.toOption.collect { case (PktLine.Frame.Data(b), _) => String(b, StandardCharsets.US_ASCII) }
      assertTrue(payload.contains("want deadbeef\n"))
    ,
    test("decodes flush, data, and flush in sequence"):
      val bytes = PktLine.flush ++ PktLine.encodeLine("NAK\n") ++ PktLine.flush
      val frames = PktLine.decodeAll(bytes)
      assertTrue(
        frames.isRight,
        frames.toOption.get.length == 3,
        frames.toOption.get.headOption.contains(PktLine.Frame.Flush),
      )
    ,
    test("reports truncated length prefixes"):
      assertTrue(PktLine.readFrame("00".getBytes(StandardCharsets.US_ASCII), 0).isLeft)
    ,
    test("reports a payload that runs past the buffer"):
      // claims 0010 (16 bytes total => 12 payload) but only provides a few
      assertTrue(PktLine.readFrame("0010ab".getBytes(StandardCharsets.US_ASCII), 0).isLeft)
  )

  private given CanEqual[PktLine.Frame, PktLine.Frame] = CanEqual.derived
