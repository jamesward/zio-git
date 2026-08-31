package com.jamesward.zio_git

import zio.Chunk

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.Instant

/** A git object id: a 40-character lowercase hex SHA-1. Constructed only
 *  through [[ObjectId.parse]] / [[ObjectId.fromRawBytes]] so an in-flight value
 *  is always well-formed (parse, don't validate). */
opaque type ObjectId = String

object ObjectId:
  private val HexLength = 40
  private val RawLength = 20

  private def isHex(s: String): Boolean =
    s.length == HexLength && s.forall(c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))

  def parse(raw: String): Option[ObjectId] =
    val lower = raw.trim.toLowerCase
    Option.when(isHex(lower))(lower)

  /** Build an id from the 20 raw bytes as they appear in trees / ref-deltas. */
  def fromRawBytes(bytes: Array[Byte]): Option[ObjectId] =
    Option.when(bytes.length == RawLength)(toHex(bytes))

  private def toHex(bytes: Array[Byte]): String =
    val sb = StringBuilder(bytes.length * 2)
    var i = 0
    while i < bytes.length do
      sb.append(f"${bytes(i) & 0xff}%02x")
      i += 1
    sb.result()

extension (id: ObjectId)
  def hex: String = id
  def abbreviated: String = id.take(7)

given CanEqual[ObjectId, ObjectId] = CanEqual.derived

/** A hexadecimal abbreviation of an object id. Git requires at least four
 *  characters before attempting unique-prefix resolution. */
opaque type ObjectIdPrefix = String

object ObjectIdPrefix:
  def parse(raw: String): Option[ObjectIdPrefix] =
    val value = raw.trim.toLowerCase
    Option.when(
      value.length >= 4 && value.length < 40 &&
        value.forall(c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))
    )(value)

extension (prefix: ObjectIdPrefix) def text: String = prefix

given CanEqual[ObjectIdPrefix, ObjectIdPrefix] = CanEqual.derived

/** A fully-qualified git ref name, e.g. `refs/heads/main`, `refs/tags/v1`, or
 *  the pseudo-ref `HEAD`. */
opaque type RefName = String

object RefName:
  val HeadPrefix = "refs/heads/"
  val TagPrefix = "refs/tags/"
  val Head: RefName = "HEAD"

  def apply(raw: String): RefName = raw

extension (name: RefName)
  def value: String = name
  def isBranch: Boolean = name.startsWith(RefName.HeadPrefix)
  def isTag: Boolean = name.startsWith(RefName.TagPrefix)
  def branchName: Option[String] = Option.when(name.isBranch)(name.stripPrefix(RefName.HeadPrefix))
  def tagName: Option[String] = Option.when(name.isTag)(name.stripPrefix(RefName.TagPrefix))

given CanEqual[RefName, RefName] = CanEqual.derived

/** The four git object types, in packfile type-code order (1..4). */
enum GitObjectType(val token: String):
  case Commit extends GitObjectType("commit")
  case Tree extends GitObjectType("tree")
  case Blob extends GitObjectType("blob")
  case Tag extends GitObjectType("tag")

object GitObjectType:
  def fromPackCode(code: Int): Option[GitObjectType] =
    code match
      case 1 => Some(Commit)
      case 2 => Some(Tree)
      case 3 => Some(Blob)
      case 4 => Some(Tag)
      case _ => None

given CanEqual[GitObjectType, GitObjectType] = CanEqual.derived

/** One advertised ref: its name and the object it points at. */
final case class Ref(name: RefName, target: ObjectId)

/** A branch (a `refs/heads/` ref) with its short name and tip commit. */
final case class Branch(name: String, commit: ObjectId)

/** A tag ref. `target` is the tagged object (a commit for a lightweight tag, or
 *  an annotated-tag object for an annotated tag). */
final case class Tag(name: String, target: ObjectId)

/** The result of ref discovery (`GET /info/refs?service=git-upload-pack`).
 *
 *  `peeled` maps a ref name (e.g. `refs/tags/v1`) to the commit its
 *  `^{}` peeled advertisement pointed at. Only annotated tags produce a
 *  peeled entry; it lets callers resolve an annotated tag straight to its
 *  target commit without a second round-trip to dereference the tag object. */
final case class RefAdvertisement(
  refs: List[Ref],
  head: Option[ObjectId],
  headTarget: Option[RefName],
  capabilities: Set[String],
  peeled: Map[RefName, ObjectId] = Map.empty,
):
  def branches: List[Branch] =
    refs.flatMap(r => r.name.branchName.map(Branch(_, r.target)))

  def tags: List[Tag] =
    refs.flatMap(r => r.name.tagName.map(Tag(_, r.target)))

  def find(name: RefName): Option[Ref] =
    refs.find(_.name == name)

  /** Resolve a ref name to the commit it ultimately points at, preferring an
   *  annotated tag's peeled target over the (tag-object) ref target. Returns
   *  `None` if the name isn't advertised. */
  def resolveToCommit(name: RefName): Option[ObjectId] =
    peeled.get(name).orElse(find(name).map(_.target))

  /** Resolve forms that can be answered from the ref advertisement alone.
   *  `None` means an abbreviated id may require fetching commit objects. */
  def resolveAdvertisedCommittish(raw: String): Either[GitError, Option[ObjectId]] =
    val committish = raw.trim
    val branch =
      committish
        .stripPrefix("refs/remotes/origin/")
        .stripPrefix("origin/")
    val named =
      Option.when(committish == "HEAD")(head).flatten
        .orElse(resolveToCommit(RefName(s"${RefName.TagPrefix}$committish")))
        .orElse(resolveToCommit(RefName(s"${RefName.HeadPrefix}$branch")))
        .orElse(resolveToCommit(RefName(committish)))
        .orElse(ObjectId.parse(committish))

    named match
      case some @ Some(_) => Right(some)
      case None =>
        ObjectIdPrefix.parse(committish) match
          case None => Right(None)
          case Some(prefix) =>
            val candidates =
              (head.toList ++ refs.flatMap(ref => resolveToCommit(ref.name)))
                .distinct
                .filter(_.hex.startsWith(prefix.text))
            candidates match
              case candidate :: Nil => Right(Some(candidate))
              case Nil              => Right(None)
              case _                => Left(GitError.AmbiguousObjectId(prefix))

/** A fully-materialized git object: its type and its (delta-resolved) content
 *  bytes. `id` is the git object id derived from the content. */
final case class RawObject(objType: GitObjectType, data: Chunk[Byte]):
  lazy val id: ObjectId =
    val bytes = data.toArray
    val header = s"${objType.token} ${bytes.length}\u0000".getBytes(StandardCharsets.US_ASCII)
    val md = MessageDigest.getInstance("SHA-1")
    md.update(header)
    md.update(bytes)
    // digest() is a fresh 20-byte array; fromRawBytes only returns None on a
    // wrong length, which cannot happen for SHA-1, so the fallback is unused.
    ObjectId.fromRawBytes(md.digest()).getOrElse(throw IllegalStateException("SHA-1 produced non-20-byte digest"))

/** A person + timestamp as recorded in a commit's `author` / `committer`
 *  header. `raw` keeps the original line for round-tripping. */
final case class PersonIdent(name: String, email: String, when: Instant, tzOffset: String, raw: String)

/** A parsed commit object. */
final case class Commit(
  id: ObjectId,
  tree: ObjectId,
  parents: List[ObjectId],
  author: PersonIdent,
  committer: PersonIdent,
  message: String,
)

/** The file mode of a tree entry, parsed from its octal ASCII token. */
enum FileMode:
  case Directory, RegularFile, ExecutableFile, SymbolicLink, GitLink

object FileMode:
  def fromToken(token: String): Option[FileMode] =
    token match
      case "40000" | "040000" => Some(Directory)
      case "100644"           => Some(RegularFile)
      case "100755"           => Some(ExecutableFile)
      case "120000"           => Some(SymbolicLink)
      case "160000"           => Some(GitLink)
      case _                  => None

given CanEqual[FileMode, FileMode] = CanEqual.derived

/** Server-side object filtering for smart-HTTP fetches. */
enum FetchFilter(val wireValue: String):
  /** Fetch commits while omitting every tree and blob. */
  case CommitsOnly extends FetchFilter("tree:0")

given CanEqual[FetchFilter, FetchFilter] = CanEqual.derived

/** Trade-off for fetching complete history. Minimal transfer omits trees and
 *  blobs when supported; server default often allows hosted providers to serve
 *  a cached pack with lower latency. */
enum HistoryFetchMode:
  case MinimalTransfer, ServerDefault

given CanEqual[HistoryFetchMode, HistoryFetchMode] = CanEqual.derived

/** One entry in a tree object. */
final case class TreeEntry(mode: FileMode, name: String, id: ObjectId)

/** A parsed tree object. */
final case class Tree(entries: List[TreeEntry])

/** Errors surfaced by the git client, carried in the ZIO typed error channel. */
enum GitError:
  case InvalidUrl(raw: String)
  case HttpError(status: Int, url: String)
  case ProtocolError(message: String)
  case ObjectNotFound(id: ObjectId)
  case RefNotFound(ref: String)
  case AmbiguousObjectId(prefix: ObjectIdPrefix)
  case Transport(cause: Throwable)
