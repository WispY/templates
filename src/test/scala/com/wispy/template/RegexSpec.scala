package com.wispy.template

import com.typesafe.scalalogging.LazyLogging
import com.wispy.template.regex._
import org.scalatest.FlatSpec

import scala.util.matching.Regex.Match

/** @author Leonid Poliakov */
class RegexSpec extends FlatSpec with LazyLogging {

  it should "find level one sections for one-deep template" in {
    val template =
      """
        |<listA>
        |  Section A
        |</listA>
        |<listB>
        |  Section B
        |</listB>
        |<listC>Section C</listC>
      """.stripMargin
    assert(flatMatches(template.findSections("<list.>", "</list.>")) == Seq("<listA>", "</listA>", "<listB>", "</listB>", "<listC>", "</listC>"))
  }

  it should "find level one sections for three-deep template" in {
    val template =
      """
        |<listA>
        |  Section A
        |  <listB>
        |    Section B
        |    <listC>Section C</listC>
        |  </listB>
        |</listA>
        |<listD>Section D</listD>
      """.stripMargin
    assert(flatMatches(template.findSections("<list.>", "</list.>")) == Seq("<listA>", "</listA>", "<listD>", "</listD>"))
  }

  it should "fail to find level one sections when closing tag is found first" in {
    val error = intercept[IllegalArgumentException]("</listA><listB></listB>".findSections("<list.>", "</list.>"))
    assert(error.getMessage == "Found unopened end token at [0 - 8]")
  }

  it should "fail to find level one sections when opening tag is not closed" in {
    val error = intercept[IllegalArgumentException]("<listA></listA><listB>".findSections("<list.>", "</list.>"))
    assert(error.getMessage == "Found unclosed start token at [15 - 22]")
  }

  private def flatMatches(sections: Seq[(Match, Match)]): Seq[String] = {
    sections.flatMap { case (start, end) => Seq(start.group(0), end.group(0)) }
  }

}
