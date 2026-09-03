zio-git
--------------------

[![javadocs.dev](https://www.javadocs.dev/com.jamesward/zio-git_3/badge.svg)](https://www.javadocs.dev/com.jamesward/zio-git_3/latest)

A small ZIO 2 / Scala 3 client for reading remote git repositories over the
**smart HTTP** protocol (`git-upload-pack`), built on [zio-http].

Scope: read-only, HTTP(S) transport only, **no authentication** (public repos).
It speaks protocol v0, negotiates side-band/filter/delta capabilities when
requested, and parses self-contained (non-thin) packfiles.

### Features

- **Ref discovery** (`refs`) — one `GET /info/refs?service=git-upload-pack`
  parsed into a typed `RefAdvertisement`: every `Ref`, the HEAD object id, the
  HEAD symref target, and the server capability set. Convenience views:
  `branches` (from `refs/heads/`) and `tags` (from `refs/tags/`, with
  annotated-tag commit targets retained in `peeled`).
- **Fetch** (`fetchObjects`) — `POST /git-upload-pack` for a set of `want`s
  (optionally shallow or server-filtered), returning a delta-resolved
  `Map[ObjectId, RawObject]`. Handles both `OFS_DELTA` and `REF_DELTA`.
- **Commit log** (`commitLog` / `log` / `fullBranchLog`) — shallow-fetch a
  slice or walk complete branch history. Callers choose `MinimalTransfer`
  (`filter tree:0` when supported) or `ServerDefault` (often a lower-latency
  provider-cached pack).
- **Committish resolution** (`resolveCommittish`) — resolves HEAD, branches,
  `origin/*`, tags (including annotated tags), full ids, and unique abbreviated
  commit ids.
- **Sparse tree read** (`readFilesUnder`) — parse a safe `RepoPath`, fetch the
  commit and tree graph with `filter blob:none`, then fetch only that subtree's
  blobs in bounded batches. `readFiles` uses the same partial-clone path for the
  full tree when the server advertises filtering, with a shallow-fetch fallback
  for servers that do not.
- **Gitignore matching** (`GitIgnore`) — pure ordered matching with negation,
  globstars, anchoring, directory rules, and parent-directory semantics.
- **Clone** (`cloneRepo`) — fetch HEAD's history and check out HEAD's tree into
  a directory (blobs written to disk, executable bit applied, submodules
  skipped).

Domain types are opaque and parsed-not-validated: `ObjectId` (40-hex SHA-1),
`RefName`, `RepoUrl`, and safe repository-relative `RepoPath`. Objects are modeled as ADTs (`GitObjectType`, `Commit`,
`Tree`, `TreeEntry`, `FileMode`) and failures as a typed `GitError`.

### Usage

```scala
import com.jamesward.zio_git.*
import zio.*
import zio.http.Client

val repo = RepoUrl.parse("https://github.com/jamesward/zio-mavencentral.git").toOption.get

val program =
  for
    adv     <- GitHttp.branches(repo)
    commits <- GitHttp.log(repo, maxCount = 10)
  yield (adv, commits)

program.provide(Client.default, GitHttp.live)
```

### Building blocks

The wire codecs are exposed as pure, independently-testable pieces:
`PktLine` (pkt-line framing), `PackFile` (packfile parsing + delta resolution),
`Delta` (git delta application), and `GitObjects` (commit/tree parsing).

[zio-http]: https://github.com/zio/zio-http
