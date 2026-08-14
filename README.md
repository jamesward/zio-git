zio-git
--------------------

A small ZIO 2 / Scala 3 client for reading remote git repositories over the
**smart HTTP** protocol (`git-upload-pack`), built on [zio-http].

Scope: read-only, HTTP(S) transport only, **no authentication** (public repos).
It speaks protocol v0, requests no `side-band`, and parses self-contained
(non-thin) packfiles.

### Features

- **Ref discovery** (`refs`) — one `GET /info/refs?service=git-upload-pack`
  parsed into a typed `RefAdvertisement`: every `Ref`, the HEAD object id, the
  HEAD symref target, and the server capability set. Convenience views:
  `branches` (from `refs/heads/`) and `tags` (from `refs/tags/`, peeled
  entries dropped).
- **Fetch** (`fetchObjects`) — `POST /git-upload-pack` for a set of `want`s
  (optionally shallow via `deepen`), returning a delta-resolved
  `Map[ObjectId, RawObject]`. Handles both `OFS_DELTA` and `REF_DELTA`.
- **Commit log** (`commitLog` / `log`) — shallow-fetch a slice and walk the
  parent graph, newest first, stopping at the shallow boundary.
- **Clone** (`cloneRepo`) — fetch HEAD's history and check out HEAD's tree into
  a directory (blobs written to disk, executable bit applied, submodules
  skipped).

Domain types are opaque and parsed-not-validated: `ObjectId` (40-hex SHA-1),
`RefName`, `RepoUrl`. Objects are modeled as ADTs (`GitObjectType`, `Commit`,
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
