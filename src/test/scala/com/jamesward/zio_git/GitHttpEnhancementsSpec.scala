package com.jamesward.zio_git

import zio.test.*

import java.nio.charset.StandardCharsets

object GitHttpEnhancementsSpec extends ZIOSpecDefault:

  private val head = ObjectId.parse("1111111111111111111111111111111111111111").get
  private val tagObject = ObjectId.parse("2222222222222222222222222222222222222222").get
  private val tagCommit = ObjectId.parse("3333333333333333333333333333333333333333").get

  private val advertisement = RefAdvertisement(
    refs = List(
      Ref(RefName("refs/heads/main"), head),
      Ref(RefName("refs/tags/v1.0.0"), tagObject),
    ),
    head = Some(head),
    headTarget = Some(RefName("refs/heads/main")),
    capabilities = Set("filter"),
    peeled = Map(RefName("refs/tags/v1.0.0") -> tagCommit),
  )

  def spec = suite("GitHttp enhancements")(
    test("advertised committish resolution covers HEAD, branch aliases, tags, ids, and prefixes"):
      val resolved = List(
        "HEAD"                     -> head,
        "main"                     -> head,
        "origin/main"              -> head,
        "refs/remotes/origin/main" -> head,
        "refs/heads/main"          -> head,
        "v1.0.0"                   -> tagCommit,
        "refs/tags/v1.0.0"         -> tagCommit,
        head.hex                    -> head,
        head.hex.take(10)           -> head,
      )
      assertTrue(
        resolved.forall: (committish, expected) =>
          advertisement.resolveAdvertisedCommittish(committish) == Right(Some(expected))
      )
    ,
    test("ambiguous abbreviated object ids are represented as a domain error"):
      val other = ObjectId.parse("1111222222222222222222222222222222222222").get
      val ambiguous = advertisement.copy(
        refs = advertisement.refs :+ Ref(RefName("refs/heads/other"), other)
      )
      val result = ambiguous.resolveAdvertisedCommittish("1111")
      val isAmbiguous = result match
        case Left(GitError.AmbiguousObjectId(prefix)) => prefix.text == "1111"
        case _                                        => false
      assertTrue(isAmbiguous)
    ,
    test("a commit-only fetch advertises filter once and sends tree:0"):
      val bytes = GitHttp.buildFetchRequest(
        List(head, tagCommit),
        deepen = None,
        filter = Some(FetchFilter.CommitsOnly),
        sideBand = true,
        ofsDelta = true,
      )
      val lines = PktLine.decodeAll(bytes).toOption.get.collect:
        case PktLine.Frame.Data(data) => String(data, StandardCharsets.US_ASCII).stripLineEnd
      assertTrue(
        lines.headOption.exists(_.startsWith(s"want ${head.hex} filter")),
        lines.lift(1).contains(s"want ${tagCommit.hex}"),
        lines.count(_.contains(" filter")) == 1,
        lines.headOption.exists(_.contains("side-band-64k")),
        lines.headOption.exists(_.contains("ofs-delta")),
        lines.contains("filter tree:0"),
        lines.lastOption.contains("done"),
      )
    ,
    test("repository paths parse only safe relative subtrees"):
      val valid = RepoPath.parse("docs/reference")
      val invalid = List("", "/docs", "docs//reference", "docs/../reference", "docs\\reference")
      assertTrue(
        valid.exists(_.value == "docs/reference"),
        invalid.forall(RepoPath.parse(_).isLeft),
      )
    ,
    test("a blobless fetch advertises filter once and sends blob:none"):
      val bytes = GitHttp.buildFetchRequest(
        List(head),
        deepen = Some(1),
        filter = Some(FetchFilter.BlobsNone),
        sideBand = true,
        ofsDelta = true,
      )
      val lines = PktLine.decodeAll(bytes).toOption.get.collect:
        case PktLine.Frame.Data(data) => String(data, StandardCharsets.US_ASCII).stripLineEnd
      assertTrue(
        lines.headOption.exists(_.startsWith(s"want ${head.hex} filter")),
        lines.contains("deepen 1"),
        lines.contains("filter blob:none"),
      )
    ,
    test("side-band parsing joins pack channel data and ignores progress"):
      val response =
        PktLine.encodeLine("NAK\n") ++
          PktLine.encode(Array(2.toByte) ++ "counting objects".getBytes(StandardCharsets.US_ASCII)) ++
          PktLine.encode(Array(1.toByte) ++ "PACK".getBytes(StandardCharsets.US_ASCII)) ++
          PktLine.encode(Array(1.toByte) ++ "DATA".getBytes(StandardCharsets.US_ASCII)) ++
          PktLine.flush
      val pack = GitHttp.parseFetchResponse(response, sideBand = true)
      assertTrue(
        pack.isRight,
        pack.toOption.exists(bytes => String(bytes, StandardCharsets.US_ASCII) == "PACKDATA"),
      )
  )
