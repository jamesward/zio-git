package com.jamesward.zio_git

import zio.Chunk
import zio.test.*

import java.nio.charset.StandardCharsets

object ProtocolSpec extends ZIOSpecDefault:

  private val sha1 = "1111111111111111111111111111111111111111"
  private val sha2 = "2222222222222222222222222222222222222222"

  private def commitObject(tree: String, parents: List[ObjectId], epoch: Long, message: String): RawObject =
    val parentLines = parents.map(p => s"parent ${p.hex}\n").mkString
    val text =
      s"tree $tree\n" +
        parentLines +
        s"author A <a@x> $epoch +0000\n" +
        s"committer A <a@x> $epoch +0000\n" +
        s"\n$message"
    RawObject(GitObjectType.Commit, Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))

  def spec = suite("Protocol")(
    test("parseAdvertisement extracts refs, HEAD, symref target, and capabilities"):
      val bytes =
        PktLine.encodeLine("# service=git-upload-pack\n") ++
          PktLine.flush ++
          PktLine.encodeLine(s"$sha1 HEAD\u0000multi_ack symref=HEAD:refs/heads/main object-format=sha1\n") ++
          PktLine.encodeLine(s"$sha1 refs/heads/main\n") ++
          PktLine.encodeLine(s"$sha2 refs/tags/v1.0.0\n") ++
          PktLine.encodeLine(s"$sha2 refs/tags/v1.0.0^{}\n") ++
          PktLine.flush
      val adv = GitHttp.parseAdvertisement(bytes)
      val parsed = adv.toOption
      assertTrue(
        adv.isRight,
        parsed.exists(_.branches.map(_.name).contains("main")),
        parsed.exists(_.tags.map(_.name).contains("v1.0.0")),
        parsed.exists(_.tags.forall(t => !t.name.endsWith("^{}"))),
        parsed.exists(_.head.exists(_.hex.equals(sha1))),
        parsed.exists(_.headTarget.exists(_.value.equals("refs/heads/main"))),
        parsed.exists(_.capabilities.contains("object-format=sha1")),
      )
    ,
    test("parseFetchResponse strips the shallow/NAK prefix and returns the raw pack"):
      val resp =
        PktLine.encodeLine(s"shallow $sha1\n") ++
          PktLine.flush ++
          PktLine.encodeLine("NAK\n") ++
          "PACKDATA".getBytes(StandardCharsets.US_ASCII)
      val pack = GitHttp.parseFetchResponse(resp)
      assertTrue(
        pack.isRight,
        pack.toOption.exists(b => String(b, StandardCharsets.US_ASCII).equals("PACKDATA")),
      )
    ,
    test("parseFetchResponse surfaces a server ERR line"):
      val resp = PktLine.encodeLine("ERR upload-pack not our ref\n")
      assertTrue(GitHttp.parseFetchResponse(resp).isLeft)
    ,
    test("walkCommits returns commits newest-first and stops at the shallow boundary"):
      val parent = commitObject(sha1, Nil, epoch = 100L, message = "parent-msg")
      val child = commitObject(sha1, List(parent.id), epoch = 200L, message = "child-msg")
      // grandparent id referenced by parent is NOT in the map => boundary
      val boundaryParent = commitObject(sha1, List(ObjectId.parse(sha2).get), epoch = 50L, message = "boundary")
      val objects = Map(parent.id -> parent, child.id -> child, boundaryParent.id -> boundaryParent)
      val walked = GitHttp.walkCommits(objects, child.id, maxCount = 10)
      val commits = walked.toOption.getOrElse(Nil)
      assertTrue(
        walked.isRight,
        commits.size == 2,
        commits.head.message.equals("child-msg"),
        commits.head.committer.when.getEpochSecond == 200L,
        commits(1).committer.when.getEpochSecond == 100L,
      )
  )
