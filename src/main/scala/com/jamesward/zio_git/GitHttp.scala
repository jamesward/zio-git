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
  def fetchObjects(
    repo: RepoUrl,
    wants: List[ObjectId],
    deepen: Option[Int] = None,
    filter: Option[FetchFilter] = None,
    sideBand: Boolean = false,
    ofsDelta: Boolean = false,
  ): ZIO[Any, GitError, Map[ObjectId, RawObject]] =
    if wants.isEmpty then ZIO.fail(GitError.ProtocolError("fetch requires at least one want"))
    else
      defer:
        val urlStr = s"${repo.base}/git-upload-pack"
        val url = decodeUrl(urlStr).run
        val reqBody = ZIO.succeed(GitHttp.buildFetchRequest(wants, deepen, filter, sideBand, ofsDelta)).run
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
        val pack = ZIO.fromEither(GitHttp.parseFetchResponse(body, sideBand)).mapError(GitError.ProtocolError.apply).run
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

  /** List every commit reachable from a branch. `MinimalTransfer` asks capable
   *  servers to omit trees and blobs; `ServerDefault` accepts the provider's
   *  normal pack, which can have lower latency when that pack is cached. Ref
   *  discovery and branch resolution share one advertisement request. */
  def fullBranchLog(
    repo: RepoUrl,
    branch: String,
    mode: HistoryFetchMode = HistoryFetchMode.MinimalTransfer,
  ): ZIO[Any, GitError, List[Commit]] =
    defer:
      val advertisement = refs(repo).run
      val start = resolveBranch(advertisement, branch).run
      val objects = fetchCommitObjects(repo, advertisement, List(start), mode).run
      ZIO.fromEither(GitHttp.walkCommits(objects, start, Int.MaxValue))
        .mapError(GitError.ProtocolError.apply).run

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
    refs(repo).flatMap: advertisement =>
      branch.fold(
        ZIO.fromOption(advertisement.head).orElseFail(GitError.RefNotFound("HEAD"))
      )(resolveBranch(advertisement, _))

  /**
   * Resolve a tag, branch, fully-qualified ref, HEAD, full object id, or unique
   * abbreviated commit id. Annotated tags are peeled from the advertisement.
   * Historical abbreviated ids require a commit-only fetch from all advertised
   * branch tips; this expensive fallback is used only when refs cannot answer.
   */
  def resolveCommittish(repo: RepoUrl, committish: String): ZIO[Any, GitError, ObjectId] =
    defer:
      val advertisement = refs(repo).run
      val advertised = ZIO.fromEither(advertisement.resolveAdvertisedCommittish(committish)).run
      advertised match
        case Some(commit) => commit
        case None =>
          val prefix = ZIO.fromOption(ObjectIdPrefix.parse(committish))
            .orElseFail(GitError.RefNotFound(committish)).run
          val wants =
            (advertisement.head.toList ++
              advertisement.branches.map(_.commit) ++
              advertisement.tags.map(_.target) ++
              advertisement.peeled.values).distinct
          ZIO.fail(GitError.RefNotFound(committish)).when(wants.isEmpty).run
          val objects = fetchCommitObjects(repo, advertisement, wants, HistoryFetchMode.MinimalTransfer).run
          val matches = objects.collect:
            case (id, obj) if obj.objType == GitObjectType.Commit && id.hex.startsWith(prefix.text) => id
          .toList
          matches match
            case commit :: Nil => commit
            case Nil           => ZIO.fail(GitError.RefNotFound(committish)).run
            case _             => ZIO.fail(GitError.AmbiguousObjectId(prefix)).run

  /**
   * Read every file in `commit`'s tree into memory, keyed by repo-relative path.
   * Servers advertising partial-clone filters are read in bounded blob batches;
   * other servers retain the original shallow full-tree fetch.
   */
  def readFiles(repo: RepoUrl, commit: ObjectId): ZIO[Any, GitError, Map[String, Chunk[Byte]]] =
    readFilesAt(repo, commit, None)

  /**
   * Read only files beneath `path`, with keys relative to that subtree. On a
   * server advertising `filter`, the first fetch requests `blob:none`; after
   * locating the subtree from the returned trees, only its blobs are fetched.
   */
  def readFilesUnder(repo: RepoUrl, commit: ObjectId, path: RepoPath): ZIO[Any, GitError, Map[String, Chunk[Byte]]] =
    readFilesAt(repo, commit, Some(path))

  private def readFilesAt(
    repo: RepoUrl,
    commit: ObjectId,
    path: Option[RepoPath],
  ): ZIO[Any, GitError, Map[String, Chunk[Byte]]] =
    defer:
      val advertisement = refs(repo).run
      if advertisement.capabilities.contains("filter") then
        readFilesSparse(repo, commit, path, advertisement).run
      else
        val all = readFilesFull(repo, commit).run
        path.fold(all)(p => GitHttp.selectSubtree(all, p))

  private def readFilesFull(repo: RepoUrl, commit: ObjectId): ZIO[Any, GitError, Map[String, Chunk[Byte]]] =
    defer:
      val objects = fetchObjects(repo, List(commit), Some(1)).run
      val commitObj = ZIO.fromOption(objects.get(commit)).orElseFail(GitError.ObjectNotFound(commit)).run
      val parsed = ZIO.fromEither(GitObjects.parseCommit(commitObj)).mapError(GitError.ProtocolError.apply).run
      ZIO.fromEither(GitHttp.collectTree(objects, parsed.tree, "")).mapError(GitError.ProtocolError.apply).run

  private def readFilesSparse(
    repo: RepoUrl,
    commit: ObjectId,
    path: Option[RepoPath],
    advertisement: RefAdvertisement,
  ): ZIO[Any, GitError, Map[String, Chunk[Byte]]] =
    defer:
      val sideBand = advertisement.capabilities.contains("side-band-64k")
      val ofsDelta = advertisement.capabilities.contains("ofs-delta")
      val trees = fetchObjects(
        repo,
        List(commit),
        deepen = Some(1),
        filter = Some(FetchFilter.BlobsNone),
        sideBand = sideBand,
        ofsDelta = ofsDelta,
      ).run
      val commitObj = ZIO.fromOption(trees.get(commit)).orElseFail(GitError.ObjectNotFound(commit)).run
      val parsed = ZIO.fromEither(GitObjects.parseCommit(commitObj)).mapError(GitError.ProtocolError.apply).run
      val subtree = ZIO.fromEither(GitHttp.resolveSubtree(trees, parsed.tree, path))
        .mapError(GitError.ProtocolError.apply).run
      val entries = ZIO.fromEither(GitHttp.collectBlobEntries(trees, subtree, ""))
        .mapError(GitError.ProtocolError.apply).run
      val blobIds = entries.map(_._2).distinct
      val batches = ZIO.succeed(blobIds.grouped(GitHttp.BlobBatchSize).toList).run
      val fetched = ZIO.foreach(batches): batch =>
        fetchObjects(repo, batch, sideBand = sideBand, ofsDelta = ofsDelta)
      .map(_.foldLeft(Map.empty[ObjectId, RawObject])(_ ++ _)).run
      ZIO.fromEither(GitHttp.materializeBlobEntries(fetched, entries))
        .mapError(GitError.ProtocolError.apply).run

  private def resolveBranch(advertisement: RefAdvertisement, branch: String): IO[GitError, ObjectId] =
    val normalized = branch.stripPrefix("refs/remotes/origin/").stripPrefix("origin/").stripPrefix(RefName.HeadPrefix)
    val name = RefName(s"${RefName.HeadPrefix}$normalized")
    ZIO.fromOption(advertisement.resolveToCommit(name)).orElseFail(GitError.RefNotFound(name.value))

  private def fetchCommitObjects(
    repo: RepoUrl,
    advertisement: RefAdvertisement,
    wants: List[ObjectId],
    mode: HistoryFetchMode,
  ): ZIO[Any, GitError, Map[ObjectId, RawObject]] =
    val filter = mode match
      case HistoryFetchMode.MinimalTransfer =>
        Option.when(advertisement.capabilities.contains("filter"))(FetchFilter.CommitsOnly)
      case HistoryFetchMode.ServerDefault => None
    val filtered = filter.isDefined
    val sideBand = filtered && advertisement.capabilities.contains("side-band-64k")
    val ofsDelta = filtered && advertisement.capabilities.contains("ofs-delta")
    fetchObjects(repo, wants, filter = filter, sideBand = sideBand, ofsDelta = ofsDelta)

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
  def fullBranchLog(
    repo: RepoUrl,
    branch: String,
    mode: HistoryFetchMode = HistoryFetchMode.MinimalTransfer,
  ): ZIO[GitHttp, GitError, List[Commit]] = ZIO.serviceWithZIO(_.fullBranchLog(repo, branch, mode))
  def cloneRepo(repo: RepoUrl, dest: File): ZIO[GitHttp, GitError, CloneResult] = ZIO.serviceWithZIO(_.cloneRepo(repo, dest))
  def resolveCommit(repo: RepoUrl, branch: Option[String]): ZIO[GitHttp, GitError, ObjectId] = ZIO.serviceWithZIO(_.resolveCommit(repo, branch))
  def resolveCommittish(repo: RepoUrl, committish: String): ZIO[GitHttp, GitError, ObjectId] = ZIO.serviceWithZIO(_.resolveCommittish(repo, committish))
  def readFiles(repo: RepoUrl, commit: ObjectId): ZIO[GitHttp, GitError, Map[String, Chunk[Byte]]] = ZIO.serviceWithZIO(_.readFiles(repo, commit))
  def readFilesUnder(repo: RepoUrl, commit: ObjectId, path: RepoPath): ZIO[GitHttp, GitError, Map[String, Chunk[Byte]]] =
    ZIO.serviceWithZIO(_.readFilesUnder(repo, commit, path))

  private[zio_git] val BlobBatchSize = 256

  private[zio_git] def selectSubtree(files: Map[String, Chunk[Byte]], path: RepoPath): Map[String, Chunk[Byte]] =
    val prefix = path.value + "/"
    files.collect:
      case (name, bytes) if name.startsWith(prefix) => name.stripPrefix(prefix) -> bytes

  private[zio_git] def resolveSubtree(
    objects: Map[ObjectId, RawObject],
    root: ObjectId,
    path: Option[RepoPath],
  ): Either[String, ObjectId] =
    path.fold[Either[String, ObjectId]](Right(root)): repoPath =>
      repoPath.segments.foldLeft[Either[String, ObjectId]](Right(root)):
        case (Left(error), _) => Left(error)
        case (Right(treeId), segment) =>
          objects.get(treeId).toRight(s"missing tree object ${treeId.hex}").flatMap: treeObject =>
            GitObjects.parseTree(treeObject).flatMap: tree =>
              tree.entries
                .find(entry => entry.mode == FileMode.Directory && entry.name == segment)
                .map(_.id)
                .toRight(s"repository path not found: ${repoPath.value}")

  private[zio_git] def collectBlobEntries(
    objects: Map[ObjectId, RawObject],
    treeId: ObjectId,
    prefix: String,
  ): Either[String, List[(String, ObjectId)]] =
    objects.get(treeId).toRight(s"missing tree object ${treeId.hex}").flatMap: treeObject =>
      GitObjects.parseTree(treeObject).flatMap: tree =>
        tree.entries.foldLeft[Either[String, List[(String, ObjectId)]]](Right(Nil)):
          case (Left(error), _) => Left(error)
          case (Right(entries), entry) =>
            entry.mode match
              case FileMode.Directory =>
                collectBlobEntries(objects, entry.id, s"$prefix${entry.name}/").map(entries ++ _)
              case FileMode.GitLink => Right(entries)
              case FileMode.RegularFile | FileMode.ExecutableFile | FileMode.SymbolicLink =>
                Right(entries :+ (s"$prefix${entry.name}" -> entry.id))

  private[zio_git] def materializeBlobEntries(
    objects: Map[ObjectId, RawObject],
    entries: List[(String, ObjectId)],
  ): Either[String, Map[String, Chunk[Byte]]] =
    entries.foldLeft[Either[String, Map[String, Chunk[Byte]]]](Right(Map.empty)):
      case (Left(error), _) => Left(error)
      case (Right(files), (path, id)) =>
        objects.get(id) match
          case Some(blob) if blob.objType == GitObjectType.Blob => Right(files.updated(path, blob.data))
          case Some(other) => Left(s"expected blob ${id.hex} for $path, got ${other.objType.token}")
          case None => Left(s"missing blob ${id.hex} for $path")

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

  /** Build a `git-upload-pack` request body. Protocol-v0 capabilities belong
   *  only on the first `want`; object filters additionally require a `filter`
   *  command before the flush. */
  def buildFetchRequest(
    wants: List[ObjectId],
    deepen: Option[Int],
    filter: Option[FetchFilter] = None,
    sideBand: Boolean = false,
    ofsDelta: Boolean = false,
  ): Array[Byte] =
    val requestedCapabilities =
      List(
        Option.when(filter.isDefined)("filter"),
        Option.when(sideBand)("side-band-64k"),
        Option.when(ofsDelta)("ofs-delta"),
      ).flatten
    val wantLines = wants.zipWithIndex.map: (want, index) =>
      val capabilities =
        Option.when(index == 0 && requestedCapabilities.nonEmpty)(" " + requestedCapabilities.mkString(" "))
          .getOrElse("")
      PktLine.encodeLine(s"want ${want.hex}$capabilities\n")
    val deepenLine = deepen.map(d => PktLine.encodeLine(s"deepen $d\n")).toList
    val filterLine = filter.map(value => PktLine.encodeLine(s"filter ${value.wireValue}\n")).toList
    val trailer = List(PktLine.flush, PktLine.encodeLine("done\n"))
    (wantLines ++ deepenLine ++ filterLine ++ trailer).foldLeft(Array.emptyByteArray)(_ ++ _)

  /** Parse a `git-upload-pack` response into raw packfile bytes. When
   *  `sideBand` is enabled, channel 1 is pack data, channel 2 is progress
   *  (discarded), and channel 3 is a server error. */
  def parseFetchResponse(bytes: Array[Byte], sideBand: Boolean = false): Either[String, Array[Byte]] =
    def packAfter(pos: Int): Either[String, Array[Byte]] =
      if sideBand then parseSideBand(bytes, pos)
      else Right(bytes.drop(pos))

    def loop(pos: Int): Either[String, Array[Byte]] =
      if pos >= bytes.length then Left("no NAK/ACK terminator before packfile")
      else
        PktLine.readFrame(bytes, pos) match
          case Left(err) => Left(err)
          case Right((frame, next)) =>
            frame match
              case PktLine.Frame.Data(payload) =>
                val text = String(payload, StandardCharsets.US_ASCII)
                if text.startsWith("NAK") || text.startsWith("ACK") then packAfter(next)
                else if text.startsWith("ERR ") then Left(s"server error: ${text.stripPrefix("ERR ").trim}")
                else loop(next) // shallow / unshallow lines
              case _ => loop(next)
    loop(0)

  private def parseSideBand(bytes: Array[Byte], start: Int): Either[String, Array[Byte]] =
    def loop(pos: Int, pack: Chunk[Byte]): Either[String, Array[Byte]] =
      if pos >= bytes.length then Right(pack.toArray)
      else
        PktLine.readFrame(bytes, pos) match
          case Left(error) => Left(error)
          case Right((PktLine.Frame.Data(payload), next)) if payload.nonEmpty =>
            payload.head match
              case 1 => loop(next, pack ++ Chunk.fromArray(payload.drop(1)))
              case 2 => loop(next, pack)
              case 3 => Left(s"server side-band error: ${String(payload.drop(1), StandardCharsets.UTF_8).trim}")
              case channel => Left(s"unknown side-band channel: $channel")
          case Right((PktLine.Frame.Data(_), _)) => Left("empty side-band packet")
          case Right((_, _))                    => Right(pack.toArray)

    loop(start, Chunk.empty)

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
      // Peeled entries ("refs/tags/x^{}") carry the *commit* an annotated tag
      // resolves to; key them by the base ref name (without the "^{}" suffix).
      val peeled = ordered.collect:
        case (id, name) if name.value.endsWith("^{}") => RefName(name.value.stripSuffix("^{}")) -> id
      .toMap
      val head = ordered.collectFirst { case (id, name) if name == RefName.Head => id }
      val headTarget = capabilities
        .collectFirst { case c if c.startsWith("symref=HEAD:") => RefName(c.stripPrefix("symref=HEAD:")) }

      Right(RefAdvertisement(refs, head, headTarget, capabilities, peeled))

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
