package com.jamesward.zio_git

import java.util.regex.{Pattern, PatternSyntaxException}

/** A parsed gitignore rule set. Rules are evaluated in source order; the last
 *  matching rule wins. A file cannot be re-included while one of its parent
 *  directories remains ignored, matching git's traversal semantics. */
final case class GitIgnore private (private val rules: Vector[GitIgnore.Rule]):

  def isIgnored(path: String, isDirectory: Boolean): Boolean =
    val parts = GitIgnore.normalize(path).split('/').filter(_.nonEmpty).toVector
    parts.indices.foldLeft(false): (parentIgnored, index) =>
      if parentIgnored then true
      else
        val candidate = parts.take(index + 1).mkString("/")
        val candidateIsDirectory = index < parts.size - 1 || isDirectory
        rules.foldLeft(Option.empty[Boolean]): (decision, rule) =>
          if rule.matches(candidate, candidateIsDirectory) then Some(!rule.negated)
          else decision
        .getOrElse(false)

object GitIgnore:

  enum ParseError:
    case InvalidPattern(pattern: String, message: String)

  private final case class Rule(
    negated: Boolean,
    directoryOnly: Boolean,
    basenameOnly: Boolean,
    pattern: Pattern,
  ):
    def matches(path: String, isDirectory: Boolean): Boolean =
      (!directoryOnly || isDirectory) &&
        pattern.matcher(if basenameOnly then path.split('/').lastOption.getOrElse("") else path).matches()

  def parse(patterns: Iterable[String]): Either[ParseError, GitIgnore] =
    patterns.foldLeft[Either[ParseError, Vector[Rule]]](Right(Vector.empty)):
      case (Left(error), _) => Left(error)
      case (Right(parsed), line) => parseRule(line).map(_.fold(parsed)(parsed :+ _))
    .map(GitIgnore.apply)

  private def parseRule(raw: String): Either[ParseError, Option[Rule]] =
    val line = raw.stripSuffix("\r")
    if line.isEmpty || line.startsWith("#") then Right(None)
    else
      val (negated, unsigned) =
        if line.startsWith("!") then true -> line.drop(1)
        else false -> line
      val unescaped =
        if unsigned.startsWith("\\#") || unsigned.startsWith("\\!") then unsigned.drop(1)
        else unsigned
      val directoryOnly = unescaped.endsWith("/") && !unescaped.endsWith("\\/")
      val withoutDirectorySuffix = if directoryOnly then unescaped.dropRight(1) else unescaped
      val anchored = withoutDirectorySuffix.startsWith("/")
      val glob = if anchored then withoutDirectorySuffix.drop(1) else withoutDirectorySuffix
      if glob.isEmpty then Right(None)
      else
        globToRegex(glob).flatMap: regex =>
          try
            Right(Some(Rule(negated, directoryOnly, !anchored && !glob.contains('/'), Pattern.compile("^" + regex + "$"))))
          catch
            case error: PatternSyntaxException => Left(ParseError.InvalidPattern(raw, error.getDescription))

  private def normalize(path: String): String =
    path.replace('\\', '/').stripPrefix("./").stripPrefix("/").stripSuffix("/")

  private def globToRegex(glob: String): Either[ParseError, String] =
    def loop(index: Int, regex: String): Either[ParseError, String] =
      if index >= glob.length then Right(regex)
      else
        glob.charAt(index) match
          case '*' if index + 1 < glob.length && glob.charAt(index + 1) == '*' =>
            val afterStars = index + 2
            if afterStars < glob.length && glob.charAt(afterStars) == '/' then
              loop(afterStars + 1, regex + "(?:.*/)?")
            else
              loop(afterStars, regex + ".*")
          case '*' => loop(index + 1, regex + "[^/]*")
          case '?' => loop(index + 1, regex + "[^/]")
          case '[' =>
            val closing = glob.indexOf(']', index + 1)
            if closing < 0 then Left(ParseError.InvalidPattern(glob, "unclosed character class"))
            else
              val rawClass = glob.substring(index + 1, closing)
              val charClass =
                if rawClass.startsWith("!") then "^" + rawClass.drop(1)
                else rawClass
              loop(closing + 1, regex + "[" + charClass + "]")
          case '\\' if index + 1 < glob.length =>
            loop(index + 2, regex + Pattern.quote(glob.charAt(index + 1).toString))
          case '\\' => loop(index + 1, regex + Pattern.quote("\\"))
          case char => loop(index + 1, regex + Pattern.quote(char.toString))

    loop(0, "")
