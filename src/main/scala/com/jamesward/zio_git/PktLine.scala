package com.jamesward.zio_git

import java.nio.charset.StandardCharsets

/**
 * The git "pkt-line" framing used by the smart-HTTP protocol. Each frame is a
 * 4-hex-digit length prefix (counting the 4 prefix bytes themselves) followed
 * by that many payload bytes. The special lengths `0000` (flush), `0001`
 * (delimiter), and `0002` (response-end) carry no payload.
 *
 * See: https://git-scm.com/docs/protocol-common#_pkt_line_format
 */
object PktLine:

  enum Frame:
    case Data(bytes: Array[Byte])
    case Flush
    case Delim
    case ResponseEnd

  private def hexNibble(b: Byte): Either[String, Int] =
    val c = (b & 0xff).toChar
    if c >= '0' && c <= '9' then Right(c - '0')
    else if c >= 'a' && c <= 'f' then Right(c - 'a' + 10)
    else if c >= 'A' && c <= 'F' then Right(c - 'A' + 10)
    else Left(s"invalid pkt-line length hex digit: '$c'")

  private def readLength(bytes: Array[Byte], offset: Int): Either[String, Int] =
    if offset + 4 > bytes.length then Left("truncated pkt-line length prefix")
    else
      for
        n0 <- hexNibble(bytes(offset))
        n1 <- hexNibble(bytes(offset + 1))
        n2 <- hexNibble(bytes(offset + 2))
        n3 <- hexNibble(bytes(offset + 3))
      yield (n0 << 12) | (n1 << 8) | (n2 << 4) | n3

  /** Read one frame starting at `offset`, returning the frame and the offset of
   *  the next frame. */
  def readFrame(bytes: Array[Byte], offset: Int): Either[String, (Frame, Int)] =
    readLength(bytes, offset).flatMap:
      case 0 => Right(Frame.Flush -> (offset + 4))
      case 1 => Right(Frame.Delim -> (offset + 4))
      case 2 => Right(Frame.ResponseEnd -> (offset + 4))
      case len if len < 4 => Left(s"invalid pkt-line length: $len")
      case len =>
        val payloadLen = len - 4
        val start = offset + 4
        val end = start + payloadLen
        if end > bytes.length then Left(s"truncated pkt-line: need $payloadLen payload bytes")
        else Right(Frame.Data(bytes.slice(start, end)) -> end)

  /** Decode every frame from `offset` to the end of the buffer. */
  def decodeAll(bytes: Array[Byte], offset: Int = 0): Either[String, List[Frame]] =
    def loop(pos: Int, acc: List[Frame]): Either[String, List[Frame]] =
      if pos >= bytes.length then Right(acc.reverse)
      else
        readFrame(bytes, pos) match
          case Left(err)               => Left(err)
          case Right((frame, nextPos)) => loop(nextPos, frame :: acc)
    loop(offset, Nil)

  /** Encode a payload as a pkt-line (length prefix + payload). */
  def encode(payload: Array[Byte]): Array[Byte] =
    val total = payload.length + 4
    val prefix = f"$total%04x".getBytes(StandardCharsets.US_ASCII)
    prefix ++ payload

  def encodeLine(line: String): Array[Byte] =
    encode(line.getBytes(StandardCharsets.US_ASCII))

  /** The flush-pkt (`0000`). */
  val flush: Array[Byte] = "0000".getBytes(StandardCharsets.US_ASCII)
