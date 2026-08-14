package com.jamesward.zio_git

import zio.*
import zio.direct.*
import zio.http.Client
import zio.test.*

import java.io.File
import java.nio.file.Files

/**
 * Integration tests against the public `jamesward/zio-mavencentral.git` repo.
 * These hit the network (no auth), so they run with the live clock and share a
 * single `Client`.
 */
object GitHttpIntegrationSpec extends ZIOSpecDefault:

  private val repo: RepoUrl =
    RepoUrl.parse("https://github.com/jamesward/zio-mavencentral.git").toOption.get

  def spec = suite("GitHttp (integration)")(
    test("lists branches including main"):
      defer:
        val branches = GitHttp.branches(repo).run
        assertTrue(branches.nonEmpty, branches.map(_.name).contains("main"))
    ,
    test("resolves HEAD and its symref target"):
      defer:
        val adv = GitHttp.refs(repo).run
        assertTrue(adv.head.isDefined, adv.headTarget.exists(_.value.equals("refs/heads/main")))
    ,
    test("lists tags for the repo"):
      defer:
        val tags = GitHttp.tags(repo).run
        assertTrue(tags.forall(t => !t.name.endsWith("^{}")))
    ,
    test("logs commits newest-first from HEAD"):
      defer:
        val commits = GitHttp.log(repo, 5).run
        val times = commits.map(_.committer.when.getEpochSecond)
        assertTrue(
          commits.nonEmpty,
          commits.size <= 5,
          times.equals(times.sortWith(_ > _)),
          commits.forall(_.author.name.nonEmpty),
        )
    ,
    test("clones HEAD's tree to disk"):
      defer:
        val dir = ZIO.attemptBlockingIO(Files.createTempDirectory("zio-git-clone").toFile)
          .mapError(GitError.Transport.apply).run
        val result = GitHttp.cloneRepo(repo, dir).run
        val hasBuildSbt = ZIO.attemptBlockingIO(File(dir, "build.sbt").exists())
          .mapError(GitError.Transport.apply).run
        assertTrue(
          result.fileCount > 0,
          result.branch.contains("main"),
          hasBuildSbt,
        )
    ,
    test("resolveCommit resolves an explicit branch and HEAD to the same id"):
      defer:
        val head = GitHttp.headCommit(repo).run
        val byBranch = GitHttp.resolveCommit(repo, Some("main")).run
        val byHead = GitHttp.resolveCommit(repo, None).run
        assertTrue(byBranch == head, byHead == head)
    ,
    test("resolveCommit fails for a non-existent branch"):
      defer:
        val result = GitHttp.resolveCommit(repo, Some("no-such-branch-xyz")).either.run
        assertTrue(result.isLeft)
    ,
    test("readFiles reads the working tree at HEAD into memory (multi-object pack)"):
      defer:
        val head = GitHttp.resolveCommit(repo, None).run
        val files = GitHttp.readFiles(repo, head).run
        assertTrue(
          files.nonEmpty,
          files.contains("build.sbt"),
          files.get("build.sbt").exists(_.nonEmpty),
        )
  ).provideShared(Client.default, GitHttp.live)
    @@ TestAspect.withLiveClock
    @@ TestAspect.sequential
    @@ TestAspect.timeout(120.seconds)
