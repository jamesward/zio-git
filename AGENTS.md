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
