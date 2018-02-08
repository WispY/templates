package com.wispy.template

import scala.util.matching.Regex.Match

/** Adds a few neat methods for regex searches.
  *
  * @author Leonid Poliakov
  */
object regex {

  implicit class RegexStringExtension(val string: String) extends AnyVal {
    /** Extracts all group ones for all matches for a given string */
    def findMatches(regex: String): Seq[String] = regex.r.findAllMatchIn(string).toSeq.map(m => m.group(1))

    /** Finds all of the first-level sections that are surrounded with startRegex and endRegex
      * Example: <foo><foo></foo></foo> will return one section from start to end if searched with <foo> and </foo>
      * Skips the second/third/etc level sections */
    def findSections(startRegex: String, endRegex: String): Seq[(Match, Match)] = {
      val starts = startRegex.r.findAllMatchIn(string).toSeq.map(s => (true, s))
      val ends = endRegex.r.findAllMatchIn(string).toSeq.map(e => (false, e))
      val all = (starts ++ ends).sortBy { case (_, token) => token.start }

      val result = all.foldLeft(Sections()) {
        // found first opening token
        case (Sections(None, level, sections), (true, token)) =>
          Sections(Some(token), level + 1, sections)
        // found closing token without opening token
        case (Sections(None, _, _), (false, token)) =>
          throw new IllegalArgumentException(s"Found unopened end token at [${token.start} - ${token.end}]")
        // found second/third/etc opening token
        case (Sections(Some(start), level, sections), (true, _)) =>
          Sections(Some(start), level + 1, sections)
        // found closing token for the first level
        case (Sections(Some(start), 1, sections), (false, token)) =>
          Sections(None, 0, sections ++ Seq((start, token)))
        // found closing token for non-first level
        case (Sections(Some(start), level, sections), (false, _)) =>
          Sections(Some(start), level - 1, sections)
      }

      result.start.foreach(token => throw new IllegalArgumentException(s"Found unclosed start token at [${token.start} - ${token.end}]"))
      result.sections
    }
  }

  private case class Sections(start: Option[Match] = None, level: Int = 0, sections: Seq[(Match, Match)] = Seq())

  implicit class RegexStringSeqExtension(val seq: Seq[String]) extends AnyVal {
    /** Filters and extracts group ones for only first matches for a given seq of strings */
    def subMatches(regex: String): Seq[String] = {
      val expression = regex.r
      seq.flatMap(s => expression.findFirstMatchIn(s).map(m => m.group(1)))
    }
  }

}
