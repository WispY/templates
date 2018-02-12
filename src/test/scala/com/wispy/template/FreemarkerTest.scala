package com.wispy.template

import java.io.StringWriter

import com.typesafe.scalalogging.LazyLogging
import freemarker.template.{Configuration, Template, Version}
import org.scalatest.FlatSpec

import scala.collection.JavaConverters._

/** @author Leonid Poliakov */
class FreemarkerTest extends FlatSpec with LazyLogging {

  it should "test" in {
    val template =
      """
        |<!-- path rendering, if-empty-else rendering -->
        |<p>Hello, ${user.firstName} ${user.middleName!".."} ${user.lastName}</p>
        |
        |<!-- list iteration -->
        |<#if attachments?has_content>
        |  <p>Attachments:</p>
        |  <ul>
        |  <#list attachments as attachment>
        |    <li>${attachment}</li>
        |  </#list>
        |  </ul>
        |</#if>
        |
        |<!-- value switch rendering -->
        |<#switch status!"">
        |  <#case "ACCEPTED">Action accepted<#break>
        |  <#case "DENIED">Action denied<#break>
        |  <#case "FAILED">Action failed<#break>
        |</#switch>
        |
        |<!-- list of lists iteration: List[List[String]] -->
        |<#list outer as inner>
        |  <#list inner as value>
        |    <li>${value}</li>
        |  </#list>
        |</#list>
        |
        |<!-- list of refs iteration: List[Map[String, String]] -->
        |<#list people as person>
        |  <li>${person.name()} - ${person.age()}</li>
        |</#list>
      """.stripMargin

    case class Person(name: String, age: Int)

    val some = Map(
      "user" -> Map("firstName" -> "John", "middleName" -> "Jr", "lastName" -> "Smit").asJava,
      "attachments" -> Seq("a1", "a2").asJava,
      "status" -> "DENIED",
      "outer" -> Seq(Seq("00", "01", "02").asJava, Seq("10", "11").asJava, Seq("20").asJava).asJava,
      "people" -> Seq(Person("Peter", 10), Person("Liza", 13)).asJava
    ).asJava

    val none = Map(
      "user" -> Map("firstName" -> "John", "lastName" -> "Smit").asJava,
      "attachments" -> Seq().asJava,
      "outer" -> Seq().asJava,
      "people" -> Seq().asJava
    ).asJava

    val config = new Configuration(Configuration.VERSION_2_3_23)
    val fmt = new Template("template", template, config)

    def process(data: AnyRef): Unit = {
      val writer = new StringWriter()
      fmt.process(data, writer)
      logger.info(s"${writer.toString}")
    }

    process(some)
    process(none)
  }

}
