package com.wispy.template

// import com.gilt.handlebars.scala.Handlebars
import com.typesafe.scalalogging.LazyLogging
import org.scalatest.FlatSpec
// import com.gilt.handlebars.scala.binding.dynamic._

/** @author Leonid Poliakov */
class HandlebarsSpec extends FlatSpec with LazyLogging {

  it should "test" in {
    // val processor = Handlebars(
    //   """
    //     |<p>Hello, my name is {{name}}. I am from {{hometown}}. I have {{kids.length}} kids:</p>
    //     |<ul>
    //     |  {{#kids}}<li>{{name}} is {{age}}</li>{{/kids}}
    //     |</ul>
    //   """.stripMargin)
    // processor.program.statements.foreach(s => logger.info(s"statement $s"))
  }

}
