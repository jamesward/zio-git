# Agent Instructions

## Agent Skills (SkillsJars)

This project pulls in Agent Skills as SkillsJars build dependencies. Before
working, extract them so they are available on the filesystem (prefer the
project's wrapper — `./sbt` — and fall back to `sbt` on PATH):

```bash
# sbt (uses skillsJarsOutputDir set in build.sbt -> .kiro/skills)
./sbt extractSkillsJars    # or: sbt extractSkillsJars
```

Read the extracted `SKILL.md` files under `.kiro/skills/` and follow any that
are relevant to the task. Currently wired:

- `com.jamesward:skills` — provides `zen-of-james` (design philosophy) and
  `zen-of-scala` (concrete Scala 3 / ZIO idioms). Follow both when writing or
  reviewing Scala/ZIO code in this repo.

To add more skills, browse https://skillsjars.com, add the dependency to
`build.sbt` in the `Skills` config, then re-run extraction. `.kiro/skills` is
gitignored — it is regenerated from the build.

## Agent tooling

- Follow the `zen-of-projects` Skill (extract with `./sbt extractSkillsJars` into the gitignored `.kiro/skills/`); this file records only project-specific facts and exceptions.
- MCP server `sbt-mcp-zio-git` (sbt-mcp) listens on `http://127.0.0.1:5118/`. Kiro uses the HTTP entry in `.kiro/settings/mcp.json`; start sbt first. Claude Code uses `.mcp.json`, which runs `.claude/sbt-mcp-stdio.sh` (approved in `.claude/settings.json`). That stdio bridge starts a foreground sbt in cloud sessions (`CLAUDE_CODE_REMOTE=true`), and locally only connects to an sbt you already started. Its tools are deferred: load them with ToolSearch (search `sbt-mcp-zio-git`). Diagnostics go to `/tmp/sbt-mcp-stdio.log` and `/tmp/sbt-mcp-server.log`.
- Maintenance routine: `.factory/MAINTENANCE.md` (weekly), following the `zen-of-projects` Skill.
