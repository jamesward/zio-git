# Agent Instructions

## Agent Skills (SkillsJars)

This project pulls in Agent Skills as SkillsJars build dependencies. Before
working, extract them so they are available on the filesystem (prefer the
project's wrapper — `./sbta` — and fall back to `sbt` on PATH):

```bash
# sbt (uses skillsJarsOutputDir set in build.sbt -> .kiro/skills)
./sbta extractSkillsJars    # or: sbt extractSkillsJars
```

Read the extracted `SKILL.md` files under `.kiro/skills/` and follow any that
are relevant to the task. Currently wired:

- `com.jamesward:skills` — provides `zen-of-james` (design philosophy) and
  `zen-of-scala` (concrete Scala 3 / ZIO idioms). Follow both when writing or
  reviewing Scala/ZIO code in this repo.

To add more skills, browse https://skillsjars.com, add the dependency to
`build.sbt` in the `Skills` config, then re-run extraction. `.kiro/skills` is
gitignored — it is regenerated from the build.

## Build and test

Library project (no `stage`). Full validation: `./sbta "Test / compile; testFull"`. Tests marked
integration talk to GitHub over the network.

## Agent tooling

- Follow the `zen-of-projects` Skill (extract with `./sbta extractSkillsJars` into the gitignored `.kiro/skills/`); this file records only project-specific facts and exceptions.
- MCP server `sbta` (this build's `./sbta --mcp`, configured in `.mcp.json` and `.kiro/settings/mcp.json`, approved in `.claude/settings.json`): use it for all sbt work: `compile`, `test` (`full` for all), `run`, `check` after editing Scala 3 sources, `glob-search` / `inspect` / `symbol-location` for symbols, `sbt-task` for any other command. Its tools are deferred in Claude Code: load them with ToolSearch (search `sbta`). If it's unavailable, say so and use `./sbta <command>`. Logs: `project/target/sbta/mcp.log`.
- Maintenance routine: `.factory/MAINTENANCE.md` (weekly), following the `zen-of-projects` Skill.

## Exceptions

- `build.sbt` doesn't enable `-Werror` (the `zen-of-projects` default): `PackFile.scala` uses the deprecated `Inflater.getTotalIn`, which would fail the build. Migrate to `getBytesRead` and then enable `-Werror`; until then this is an intentional gap.
