package com.wispy.template

import java.io.{PrintWriter, StringWriter}

import com.typesafe.scalalogging.LazyLogging
import org.fusesource.scalate.{DefaultRenderContext, TemplateEngine}
import org.scalatest.FlatSpec

/** @author Leonid Poliakov */
class MustacheTest extends FlatSpec with LazyLogging {

  val engine = new TemplateEngine()

  ignore should "render section with parent leaf" in {
    render("{{#user}}{{parent}}{{/user}}", Map("user" -> Some("user"), "parent" -> "parent"))
  }

  ignore should "render inverted section with parent leaf" in {
    render("{{^user}}{{emptyMessage}}{{/user}}", Map("user" -> None, "emptyMessage" -> "EMPTY"))
  }

  ignore should "render list of leaves" in {
    render("{{#users}}{{.}}{{/users}}", Map("users" -> List("John", "Smit")))
  }

  ignore should "render self-ref" in {
    render("{{.}}", Map("users" -> List("John", "Smit")))
  }

  private def render(template: String, data: Map[String, Any]): Unit = {
    val compiled = engine.compileMoustache(template)
    val out = new StringWriter()
    val context = new DefaultRenderContext("index.html", engine, new PrintWriter(out))
    data.foreach { case (key, value) => context.attributes.update(key, value) }
    compiled.render(context)
    logger.info(s"$out")
  }

  it should "test" in {
    val template =
      """
        |<!-- path rendering, if-empty-else rendering -->
        |<p>Hello,
        |  {{#user}}
        |    <span>{{firstName}}</span>
        |    <span>{{middleName}}{{^middleName}}..{{/middleName}}</span>
        |    <span>{{lastName}}</span>
        |  {{/user}}
        |</p>
        |
        |<!-- list iteration -->
        |<!-- note: non-empty check is not supported without iteration -->
        |<p>Attachments:</p>
        |<ul>
        |  {{#attachments}}<li>{{.}}</li>{{/attachments}}
        |</ul>
        |
        |<!-- value switch rendering -->
        |<!-- not supported -->
        |
        |<!-- list of lists iteration: List[List[String]] -->
        |{{#outer}}
        |  {{#.}}
        |    <li>{{.}}</li>
        |  {{/.}}
        |{{/outer}}
        |
        |<!-- list of refs iteration: List[Map[String, String]] -->
        |{{#people}}
        |  <li>{{name}} - {{age}}</li>
        |{{/people}}
      """.stripMargin

    case class Person(name: String, age: Int)

    val some = Map(
      "user" -> Map("firstName" -> "John", "middleName" -> "Jr", "lastName" -> "Smit"),
      "attachments" -> Seq("a1", "a2"),
      "status" -> "DENIED",
      "outer" -> Seq(Seq("00", "01", "02"), Seq("10", "11"), Seq("20")),
      "people" -> Seq(Person("Peter", 10), Person("Liza", 13))
    )

    val none = Map(
      "user" -> Map("firstName" -> "John", "lastName" -> "Smit"),
      "attachments" -> Seq(),
      "outer" -> Seq(),
      "people" -> Seq()
    )

    render(template, some)
    render(template, none)
  }

}
