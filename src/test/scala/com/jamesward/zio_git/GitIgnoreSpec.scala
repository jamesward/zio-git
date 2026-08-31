package com.jamesward.zio_git

import zio.test.*

object GitIgnoreSpec extends ZIOSpecDefault:

  private def matcher(patterns: String*): GitIgnore =
    GitIgnore.parse(patterns).toOption.get

  def spec = suite("GitIgnore")(
    test("matches double-star paths and excludes descendants through their parent"):
      val ignore = matcher("**/foo")
      assertTrue(
        ignore.isIgnored("foo", isDirectory = true),
        !ignore.isIgnored("test", isDirectory = false),
        !ignore.isIgnored("test/test", isDirectory = false),
        ignore.isIgnored("foo/test", isDirectory = false),
        ignore.isIgnored("foo/foo", isDirectory = true),
        ignore.isIgnored("foo/foo/test", isDirectory = false),
      )
    ,
    test("a basename pattern matches at every depth"):
      val ignore = matcher("*.foo")
      assertTrue(
        ignore.isIgnored("asdf.foo", isDirectory = false),
        ignore.isIgnored("foo/asdf.foo", isDirectory = false),
        !ignore.isIgnored("foo/test", isDirectory = false),
      )
    ,
    test("supports hidden-file, anchored, directory-only, and character-class patterns"):
      val ignore = matcher("**/.*", "/root-only", "build/", "file[0-9].txt")
      assertTrue(
        ignore.isIgnored(".foo", isDirectory = false),
        ignore.isIgnored("foo/.foo", isDirectory = false),
        ignore.isIgnored("root-only", isDirectory = false),
        !ignore.isIgnored("nested/root-only", isDirectory = false),
        ignore.isIgnored("build/output.js", isDirectory = false),
        !ignore.isIgnored("build", isDirectory = false),
        ignore.isIgnored("nested/file7.txt", isDirectory = false),
      )
    ,
    test("last matching rule wins but a child cannot be included below an ignored parent"):
      val included = matcher("*.log", "!keep.log")
      val blockedByParent = matcher("private/", "!private/keep.txt")
      val includedParent = matcher("private/", "!private/", "private/*", "!private/keep.txt")
      assertTrue(
        included.isIgnored("drop.log", isDirectory = false),
        !included.isIgnored("keep.log", isDirectory = false),
        blockedByParent.isIgnored("private/keep.txt", isDirectory = false),
        !includedParent.isIgnored("private/keep.txt", isDirectory = false),
      )
    ,
    test("ignores comments and supports escaped comment and negation prefixes"):
      val ignore = matcher("# comment", "", "\\#generated", "\\!important")
      assertTrue(
        ignore.isIgnored("#generated", isDirectory = false),
        ignore.isIgnored("!important", isDirectory = false),
        !ignore.isIgnored("comment", isDirectory = false),
      )
  )
