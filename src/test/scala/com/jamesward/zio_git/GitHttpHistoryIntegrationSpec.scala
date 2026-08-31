package com.jamesward.zio_git

import zio.*
import zio.direct.*
import zio.http.Client
import zio.test.*

object GitHttpHistoryIntegrationSpec extends ZIOSpecDefault:

  private val repo = RepoUrl.parse("https://github.com/jamesward/zio-git.git").toOption.get

  def spec = suite("GitHttp full history (integration)")(
    test("fetches complete branch history"):
      defer:
        val commits = GitHttp.fullBranchLog(repo, "main").run
        assertTrue(
          commits.size > 1,
          commits.forall(_.message.nonEmpty),
        )
  ).provideShared(Client.default, GitHttp.live)
    @@ TestAspect.withLiveClock
    @@ TestAspect.timeout(2.minutes)
