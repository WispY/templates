package com.wispy.template

import java.util.Locale

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.FlatSpec
import org.thymeleaf.TemplateEngine
import org.thymeleaf.context.Context
import org.thymeleaf.templateresolver.StringTemplateResolver

import scala.beans.BeanProperty
import scala.collection.JavaConverters._

/** @author Leonid Poliakov */
class ThymeleafTest extends FlatSpec with LazyLogging {

  it should "test" in {
    val template =
      """
        |<!-- path rendering, if-empty-else rendering -->
        |<p>Hello,
        |  <span th:text="${user.firstName}">Mona</span>
        |  <span th:text="${user.middleName == null} ? '..' : ${user.middleName}">M</span>
        |  <span th:text="${user.lastName}">Lisa</span>
        |</p>
        |
        |<!-- list iteration -->
        |<div th:if="${attachments != null && !attachments.isEmpty}">
        |  <p>Attachments:</p>
        |  <ul>
        |    <li th:each="attachment: ${attachments}" th:text=${attachment}>Attachment</li>
        |  </ul>
        |</div>
        |
        |<!-- value switch rendering -->
        |<th:block th:switch="${status}">
        |  <p th:case="'ACCEPTED'">Action accepted</p>
        |  <p th:case="'DENIED'">Action denied</p>
        |  <p th:case="'FAILED'">Action failed</p>
        |</th:block>
        |
        |<!-- list of lists iteration: List[List[String]] -->
        |<th:block th:each="inner: ${outer}">
        |  <li th:each="value: ${inner}" th:text="${value}">Inner value</li>
        |</th:block>
        |
        |<!-- list of refs iteration: List[Map[String, String]] -->
        |<th:block th:each="person: ${people}">
        |  <li><span th:text="${person.name}">Name</span> - <span th:text="${person.age}">Age</span></li>
        |</th:block>
      """.stripMargin

    @BeanProperty
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

    val resolver = new StringTemplateResolver()
    val engine = new TemplateEngine()
    engine.setTemplateResolver(resolver)
    logger.info(engine.process(template, new Context(Locale.getDefault, some)))
    logger.info(engine.process(template, new Context(Locale.getDefault, none)))
  }

  //    engine.setDialect(new StandardDialect() {
  //      override def getExpressionParser: IStandardExpressionParser = {
  //        val delegate = super.getExpressionParser
  //        (context: IExpressionContext, input: String) => {
  //          logger.info(s"raw expression [$input]")
  //          val expression = delegate.parseExpression(context, input)
  //          logger.info(s"parsed expression [$expression] [${expression.getClass}]")
  //          expression
  //        }
  //      }
  //    })

}
