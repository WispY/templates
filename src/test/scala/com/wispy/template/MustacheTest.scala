package com.wispy.template

import java.io.{PrintWriter, StringWriter}

import com.typesafe.scalalogging.LazyLogging
import org.fusesource.scalate.{DefaultRenderContext, TemplateEngine}
import org.scalatest.FlatSpec

/** @author Leonid Poliakov */
class MustacheTest extends FlatSpec with LazyLogging {

  val engine = new TemplateEngine()

  it should "render section with parent leaf" in {
    render("{{#user}}{{parent}}{{/user}}", Map("user" -> Some("user"), "parent" -> "parent"))
  }

  it should "render inverted section with parent leaf" in {
    render("{{^user}}{{emptyMessage}}{{/user}}", Map("user" -> None, "emptyMessage" -> "EMPTY"))
  }

  it should "render list of leaves" in {
    render("{{#users}}{{.}}{{/users}}", Map("users" -> List("John", "Smit")))
  }

  it should "render self-ref" in {
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

}
