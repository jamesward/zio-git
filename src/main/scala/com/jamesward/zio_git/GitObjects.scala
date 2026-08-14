package com.jamesward.zio_git

import zio.Chunk

import java.nio.charset.StandardCharsets
import java.time.Instant

/** Parsers for the textual `commit` object and the binary `tree` object. */
object GitObjects:

  /**
   * Parse a commit object. A commit is a header block (`tree`, zero or more
   * `parent`, `author`, `committer`, plus optional extras like `gpgsig`),
   * a blank line, then the message.
   */
  def parseCommit(obj: RawObject): Either[String, Commit] =
    if obj.objType != GitObjectType.Commit then
      Left(s"expected a commit object, got ${obj.objType.token}")
    else
      val text = String(obj.data.toArray, StandardCharsets.UTF_8)
      val separator = text.indexOf("\n\n")
      val (headerPart, message) =
        if separator < 0 then (text, "")
        else (text.substring(0, separator), text.substring(separator + 2))
      val headers = headerPart.split('\n').toList

      def headerValue(key: String): Option[String] =
        val prefix = s"$key "
        headers.find(_.startsWith(prefix)).map(_.substring(prefix.length))

      val parents = headers.filter(_.startsWith("parent ")).flatMap: line =>
        ObjectId.parse(line.substring("parent ".length))

      for
        treeStr <- headerValue("tree").toRight("commit is missing a tree header")
        tree <- ObjectId.parse(treeStr).toRight(s"invalid tree id: $treeStr")
        authorLine <- headerValue("author").toRight("commit is missing an author header")
        committerLine <- headerValue("committer").toRight("commit is missing a committer header")
        author <- parsePerson(authorLine)
        committer <- parsePerson(committerLine)
      yield Commit(obj.id, tree, parents, author, committer, message)

  // `Name <email> <epoch-seconds> <±hhmm>`
  private def parsePerson(line: String): Either[String, PersonIdent] =
    val emailStart = line.indexOf('<')
    val emailEnd = line.indexOf('>')
    if emailStart < 0 || emailEnd < emailStart then Left(s"malformed person line: $line")
    else
      val name = line.substring(0, emailStart).trim
      val email = line.substring(emailStart + 1, emailEnd)
      val rest = line.substring(emailEnd + 1).trim.split(' ').filter(_.nonEmpty)
      val when = rest.headOption.flatMap(_.toLongOption).map(Instant.ofEpochSecond)
      val tz = rest.lift(1).getOrElse("")
      when match
        case Some(instant) => Right(PersonIdent(name, email, instant, tz, line))
        case None          => Right(PersonIdent(name, email, Instant.EPOCH, tz, line))

  /**
   * Parse a tree object: a sequence of `<octal-mode> <name>\0<20-byte-sha>`
   * entries with no separators between them.
   */
  def parseTree(obj: RawObject): Either[String, Tree] =
    if obj.objType != GitObjectType.Tree then
      Left(s"expected a tree object, got ${obj.objType.token}")
    else
      val bytes = obj.data.toArray

      def loop(pos: Int, acc: List[TreeEntry]): Either[String, List[TreeEntry]] =
        if pos >= bytes.length then Right(acc.reverse)
        else
          val space = bytes.indexOf(' '.toByte, pos)
          val nul = bytes.indexOf(0.toByte, space)
          if space < 0 || nul < 0 then Left("malformed tree entry header")
          else
            val modeToken = String(bytes.slice(pos, space), StandardCharsets.US_ASCII)
            val name = String(bytes.slice(space + 1, nul), StandardCharsets.UTF_8)
            val idBytes = bytes.slice(nul + 1, nul + 21)
            (FileMode.fromToken(modeToken), ObjectId.fromRawBytes(idBytes)) match
              case (Some(mode), Some(id)) => loop(nul + 21, TreeEntry(mode, name, id) :: acc)
              case (None, _)              => Left(s"unknown tree entry mode: $modeToken")
              case (_, None)              => Left("tree entry sha was not 20 bytes")

      loop(0, Nil).map(Tree.apply)

  extension (bytes: Array[Byte])
    private def indexOf(target: Byte, from: Int): Int =
      var i = from
      var found = -1
      while i < bytes.length && found < 0 do
        if bytes(i) == target then found = i
        i += 1
      found
