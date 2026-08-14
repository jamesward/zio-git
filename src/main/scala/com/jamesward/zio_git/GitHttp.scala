package com.jamesward.zio_git

import zio.{Tag as _, Ref as _, *}
import zio.direct.*
import zio.http.*

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

/** A base "smart HTTP" git repository URL, e.g.
 *  `https://github.com/owner/repo.git`. Normalized without a trailing slash. */
opaque type RepoUrl = String

object RepoUrl:
  def parse(raw: String): Either[GitError.InvalidUrl, RepoUrl] =
    val trimmed = raw.trim.stripSuffix("/")
    if trimmed.startsWith("http://") || trimmed.startsWith("https://") then Right(trimmed)
    else Left(GitError.InvalidUrl(raw))

extension (repo: RepoUrl) def base: String = repo

given CanEqual[RepoUrl, RepoUrl] = CanEqual.derived

/** The outcome of a [[GitHttp.cloneRepo]]: the checked-out HEAD commit and how
 *  many blobs were written to the working directory. */
final case class CloneResult(head: ObjectId, branch: Option[String], headCommit: Commit, fileCount: Int)

/**
 * A read-only git client speaking the smart-HTTP `git-upload-pack` protocol
 * over ZIO HTTP. Supports ref discovery (branches, tags, HEAD), fetching &
 * parsing packfiles, listing commits, and cloning (checking out HEAD's tree).
 *
 * Only `http(s)` transport with no authentication is supported.
 */
final case class GitHttp(client: Client):

  import GitHttp.UserAgent

  /** Ref discovery: `GET /info/refs?service=git-upload-pack`. */
  def refs(repo: RepoUrl): ZIO[Any, GitError, RefAdvertisement] =
    defer:
      val urlStr = s"${repo.base}/info/refs?service=git-upload-pack"
      val url = decodeUrl(urlStr).run
      val request = Request.get(url).addHeader("User-Agent", UserAgent)
      val response = client.batched(request).mapError(GitError.Transport.apply).run
      ZIO.fail(GitError.HttpError(response.status.code, urlStr)).when(!response.status.isSuccess).run
      val body = response.body.asArray.mapError(GitError.Transport.apply).run
      ZIO.fromEither(GitHttp.parseAdvertisement(body)).mapError(GitError.ProtocolError.apply).run

  def branches(repo: RepoUrl): ZIO[Any, GitError, List[Branch]] = refs(repo).map(_.branches)

  def tags(repo: RepoUrl): ZIO[Any, GitError, List[Tag]] = refs(repo).map(_.tags)

  /** Resolve the commit id that HEAD points at. */
  def headCommit(repo: RepoUrl): ZIO[Any, GitError, ObjectId] =
    refs(repo).flatMap(adv => ZIO.fromOption(adv.head).orElseFail(GitError.RefNotFound("HEAD")))

  /**
   * Fetch objects reachable from `wants` via `POST /git-upload-pack`, returning
   * the delta-resolved object map. `deepen` requests a shallow history of that
   * many commits.
   */
  def fetchObjects(repo: RepoUrl, wants: List[ObjectId], deepen: Option[Int] = None): ZIO[Any, GitError, Map[ObjectId, RawObject]] =
    if wants.isEmpty then ZIO.fail(GitError.ProtocolError("fetch requires at least one want"))
    else
      defer:
        val urlStr = s"${repo.base}/git-upload-pack"
        val url = decodeUrl(urlStr).run
        val reqBody = ZIO.succeed(GitHttp.buildFetchRequest(wants, deepen)).run
        val request =
          ZIO.succeed(
            Request
              .post(url, Body.fromArray(reqBody))
              .addHeader("Content-Type", "application/x-git-upload-pack-request")
              .addHeader("Accept", "application/x-git-upload-pack-result")
              .addHeader("User-Agent", UserAgent)
          ).run
        val response = client.batched(request).mapError(GitError.Transport.apply).run
        ZIO.fail(GitError.HttpError(response.status.code, urlStr)).when(!response.status.isSuccess).run
        val body = response.body.asArray.mapError(GitError.Transport.apply).run
        val pack = ZIO.fromEither(GitHttp.parseFetchResponse(body)).mapError(GitError.ProtocolError.apply).run
        ZIO.fromEither(PackFile.parse(pack)).mapError(GitError.ProtocolError.apply).run

  /**
   * List up to `maxCount` commits reachable from `start`, newest first. Fetches
   * a shallow slice (`deepen maxCount`) then walks the parent graph; the walk
   * stops at the shallow boundary (parents not present in the pack).
   */
  def commitLog(repo: RepoUrl, start: ObjectId, maxCount: Int = 50): ZIO[Any, GitError, List[Commit]] =
    defer:
      val objects = fetchObjects(repo, List(start), Some(maxCount)).run
      ZIO.fromEither(GitHttp.walkCommits(objects, start, maxCount)).mapError(GitError.ProtocolError.apply).run

  /** Convenience: list commits reachable from the default branch (HEAD). */
  def log(repo: RepoUrl, maxCount: Int = 50): ZIO[Any, GitError, List[Commit]] =
    headCommit(repo).flatMap(commitLog(repo, _, maxCount))

  /**
   * Clone the repository by fetching HEAD's full history and checking out
   * HEAD's tree into `dest`. Submodules (gitlinks) are skipped.
   */
  def cloneRepo(repo: RepoUrl, dest: File): ZIO[Any, GitError, CloneResult] =
    defer:
      val adv = refs(repo).run
      val headSha = ZIO.fromOption(adv.head).orElseFail(GitError.RefNotFound("HEAD")).run
      val objects = fetchObjects(repo, List(headSha), None).run
      val headObj = ZIO.fromOption(objects.get(headSha)).orElseFail(GitError.ObjectNotFound(headSha)).run
      val commit = ZIO.fromEither(GitObjects.parseCommit(headObj)).mapError(GitError.ProtocolError.apply).run
      val fileCount = checkoutTree(objects, commit.tree, dest).run
      CloneResult(headSha, adv.headTarget.flatMap(_.branchName), commit, fileCount)

  /** Resolve a branch's tip commit id, or HEAD's when `branch` is None. */
  def resolveCommit(repo: RepoUrl, branch: Option[String]): ZIO[Any, GitError, ObjectId] =
    refs(repo).flatMap: adv =>
      branch match
        case None =>
          ZIO.fromOption(adv.head).orElseFail(GitError.RefNotFound("HEAD"))
        case Some(b) =>
          val name = RefName(s"${RefName.HeadPrefix}$b")
          ZIO.fromOption(adv.find(name).map(_.target)).orElseFail(GitError.RefNotFound(name.value))

  /**
   * Read every file in `commit`'s tree into memory, keyed by repo-relative path
   * (e.g. `docs/intro.md`). Shallow-fetches just that commit (`deepen 1`), so no
   * history is transferred and nothing is written to disk. Submodules (gitlinks)
   * are skipped.
   */
  def readFiles(repo: RepoUrl, commit: ObjectId): ZIO[Any, GitError, Map[String, Chunk[Byte]]] =
    defer:
      val objects = fetchObjects(repo, List(commit), Some(1)).run
      val commitObj = ZIO.fromOption(objects.get(commit)).orElseFail(GitError.ObjectNotFound(commit)).run
      val c = ZIO.fromEither(GitObjects.parseCommit(commitObj)).mapError(GitError.ProtocolError.apply).run
      ZIO.fromEither(GitHttp.collectTree(objects, c.tree, "")).mapError(GitError.ProtocolError.apply).run

  private def checkoutTree(objects: Map[ObjectId, RawObject], treeId: ObjectId, dir: File): ZIO[Any, GitError, Int] =
    defer:
      val treeObj = ZIO.fromOption(objects.get(treeId)).orElseFail(GitError.ObjectNotFound(treeId)).run
      val tree = ZIO.fromEither(GitObjects.parseTree(treeObj)).mapError(GitError.ProtocolError.apply).run
      ZIO.attemptBlockingIO(Files.createDirectories(dir.toPath)).mapError(GitError.Transport.apply).run
      val counts = ZIO.foreach(tree.entries)(entry => writeEntry(objects, entry, dir)).run
      counts.sum

  private def writeEntry(objects: Map[ObjectId, RawObject], entry: TreeEntry, dir: File): ZIO[Any, GitError, Int] =
    entry.mode match
      case FileMode.Directory =>
        checkoutTree(objects, entry.id, File(dir, entry.name))
      case FileMode.GitLink =>
        // Submodule reference; there is no object to check out here.
        ZIO.succeed(0)
      case FileMode.RegularFile | FileMode.ExecutableFile | FileMode.SymbolicLink =>
        writeBlob(objects, entry, dir)

  private def writeBlob(objects: Map[ObjectId, RawObject], entry: TreeEntry, dir: File): ZIO[Any, GitError, Int] =
    defer:
      val blob = ZIO.fromOption(objects.get(entry.id)).orElseFail(GitError.ObjectNotFound(entry.id)).run
      val target = File(dir, entry.name)
      ZIO.attemptBlockingIO:
        Files.write(target.toPath, blob.data.toArray)
        if entry.mode == FileMode.ExecutableFile then target.setExecutable(true, false): Unit
      .mapError(GitError.Transport.apply).run
      1

  private def decodeUrl(raw: String): ZIO[Any, GitError, URL] =
    ZIO.fromEither(URL.decode(raw)).mapError(_ => GitError.InvalidUrl(raw))

object GitHttp:

  val UserAgent = "git/zio-git"

  val live: ZLayer[Client, Nothing, GitHttp] = ZLayer.fromFunction(GitHttp.apply)

  def refs(repo: RepoUrl): ZIO[GitHttp, GitError, RefAdvertisement] = ZIO.serviceWithZIO(_.refs(repo))
  def branches(repo: RepoUrl): ZIO[GitHttp, GitError, List[Branch]] = ZIO.serviceWithZIO(_.branches(repo))
  def tags(repo: RepoUrl): ZIO[GitHttp, GitError, List[Tag]] = ZIO.serviceWithZIO(_.tags(repo))
  def headCommit(repo: RepoUrl): ZIO[GitHttp, GitError, ObjectId] = ZIO.serviceWithZIO(_.headCommit(repo))
  def log(repo: RepoUrl, maxCount: Int = 50): ZIO[GitHttp, GitError, List[Commit]] = ZIO.serviceWithZIO(_.log(repo, maxCount))
  def cloneRepo(repo: RepoUrl, dest: File): ZIO[GitHttp, GitError, CloneResult] = ZIO.serviceWithZIO(_.cloneRepo(repo, dest))
  def resolveCommit(repo: RepoUrl, branch: Option[String]): ZIO[GitHttp, GitError, ObjectId] = ZIO.serviceWithZIO(_.resolveCommit(repo, branch))
  def readFiles(repo: RepoUrl, commit: ObjectId): ZIO[GitHttp, GitError, Map[String, Chunk[Byte]]] = ZIO.serviceWithZIO(_.readFiles(repo, commit))

  /** Recursively collect every blob under `treeId` into a path->bytes map,
   *  `prefix` being the accumulated directory path. Submodules are skipped. */
  private[zio_git] def collectTree(
    objects: Map[ObjectId, RawObject],
    treeId: ObjectId,
    prefix: String,
  ): Either[String, Map[String, Chunk[Byte]]] =
    objects.get(treeId) match
      case None => Left(s"missing tree object ${treeId.hex}")
      case Some(obj) =>
        GitObjects.parseTree(obj).flatMap: tree =>
          tree.entries.foldLeft[Either[String, Map[String, Chunk[Byte]]]](Right(Map.empty)):
            case (Left(e), _) => Left(e)
            case (Right(acc), entry) =>
              entry.mode match
                case FileMode.Directory =>
                  collectTree(objects, entry.id, s"$prefix${entry.name}/").map(acc ++ _)
                case FileMode.GitLink =>
                  Right(acc)
                case FileMode.RegularFile | FileMode.ExecutableFile | FileMode.SymbolicLink =>
                  objects.get(entry.id) match
                    case Some(blob) => Right(acc + (s"$prefix${entry.name}" -> blob.data))
                    case None       => Left(s"missing blob ${entry.id.hex} for $prefix${entry.name}")

  /** Build a `git-upload-pack` request body: `want` lines, an optional
   *  `deepen`, a flush, then `done`. */
  def buildFetchRequest(wants: List[ObjectId], deepen: Option[Int]): Array[Byte] =
    val wantLines = wants.map(w => PktLine.encodeLine(s"want ${w.hex}\n"))
    val deepenLine = deepen.map(d => PktLine.encodeLine(s"deepen $d\n")).toList
    val trailer = List(PktLine.flush, PktLine.encodeLine("done\n"))
    (wantLines ++ deepenLine ++ trailer).reduce(_ ++ _)

  /**
   * Parse a `git-upload-pack` response into the raw packfile bytes. We advertise
   * no `side-band` capability, so after the negotiation pkt-lines (`shallow`
   * updates and the terminating `NAK`/`ACK`) the packfile follows verbatim.
   */
  def parseFetchResponse(bytes: Array[Byte]): Either[String, Array[Byte]] =
    def loop(pos: Int): Either[String, Array[Byte]] =
      if pos >= bytes.length then Left("no NAK/ACK terminator before packfile")
      else
        PktLine.readFrame(bytes, pos) match
          case Left(err) => Left(err)
          case Right((frame, next)) =>
            frame match
              case PktLine.Frame.Data(payload) =>
                val text = String(payload, StandardCharsets.US_ASCII)
                if text.startsWith("NAK") || text.startsWith("ACK") then Right(bytes.drop(next))
                else if text.startsWith("ERR ") then Left(s"server error: ${text.stripPrefix("ERR ").trim}")
                else loop(next) // shallow / unshallow lines
              case _ => loop(next)
    loop(0)

  /** Parse an `info/refs` advertisement into a [[RefAdvertisement]]. */
  def parseAdvertisement(bytes: Array[Byte]): Either[String, RefAdvertisement] =
    PktLine.decodeAll(bytes).flatMap: frames =>
      val lines = frames.collect { case PktLine.Frame.Data(b) => String(b, StandardCharsets.UTF_8) }
        .map(_.stripLineEnd)
        .filterNot(l => l.isEmpty || l.startsWith("# service="))

      val (refEntries, capabilities) = lines.foldLeft((List.empty[(ObjectId, RefName)], Set.empty[String])):
        case ((accRefs, accCaps), line) =>
          val nul = line.indexOf('\u0000')
          val (refPart, caps) =
            if nul >= 0 then (line.substring(0, nul), line.substring(nul + 1).split(' ').filter(_.nonEmpty).toSet)
            else (line, Set.empty[String])
          parseRefLine(refPart) match
            case Some(ref) => (ref :: accRefs, accCaps ++ caps)
            case None      => (accRefs, accCaps ++ caps)

      val ordered = refEntries.reverse
      // Drop peeled tag entries ("refs/tags/x^{}") and the HEAD pseudo-ref.
      val refs = ordered.collect:
        case (id, name) if name != RefName.Head && !name.value.endsWith("^{}") => Ref(name, id)
      val head = ordered.collectFirst { case (id, name) if name == RefName.Head => id }
      val headTarget = capabilities
        .collectFirst { case c if c.startsWith("symref=HEAD:") => RefName(c.stripPrefix("symref=HEAD:")) }

      Right(RefAdvertisement(refs, head, headTarget, capabilities))

  private def parseRefLine(refPart: String): Option[(ObjectId, RefName)] =
    val space = refPart.indexOf(' ')
    if space < 0 then None
    else
      ObjectId.parse(refPart.substring(0, space)).map: id =>
        id -> RefName(refPart.substring(space + 1))

  /** Walk the commit graph from `start`, newest first, up to `maxCount`. */
  def walkCommits(objects: Map[ObjectId, RawObject], start: ObjectId, maxCount: Int): Either[String, List[Commit]] =
    def parseAt(id: ObjectId): Either[String, Option[Commit]] =
      objects.get(id) match
        case Some(obj) if obj.objType == GitObjectType.Commit => GitObjects.parseCommit(obj).map(Some(_))
        case _                                                => Right(None) // absent = shallow boundary

    def loop(frontier: List[ObjectId], visited: Set[ObjectId], acc: List[Commit]): Either[String, List[Commit]] =
      frontier match
        case Nil => Right(acc)
        case id :: rest if visited.contains(id) => loop(rest, visited, acc)
        case id :: rest =>
          parseAt(id) match
            case Left(err)         => Left(err)
            case Right(None)       => loop(rest, visited + id, acc)
            case Right(Some(commit)) => loop(rest ++ commit.parents, visited + id, commit :: acc)

    loop(List(start), Set.empty, Nil)
      .map(_.sortBy(c => -c.committer.when.getEpochSecond).take(maxCount))
