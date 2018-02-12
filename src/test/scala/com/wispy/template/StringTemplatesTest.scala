package com.wispy.template

import com.typesafe.scalalogging.LazyLogging
import org.clapper.scalasti.ST
import org.scalatest.FlatSpec

/** @author Leonid Poliakov */
class StringTemplatesTest extends FlatSpec with LazyLogging {

  it should "test" in {
    val template =
      """
        |<!-- path rendering, if-empty-else rendering -->
        |<p>Hello, $user.firstName$ $if(user.middleName)$ $user.middleName$ $else$ .. $endif$ $user.lastName$!</p>
        |
        |<!-- list iteration -->
        |<!-- note: list cannot be undefined/null -->
        |$if(attachments)$
        |  <p>Attachments:</p>
        |  <ul>
        |    $attachments:{x |
        |      <li>$x$</li>
        |    }$
        |  </ul>
        |$endif$
        |
        |<!-- value switch rendering -->
        |<!-- note: not available, boolean switch is present -->
        |
        |<!-- list of lists iteration: List[List[String]] -->
        |<!-- note: does not work -->
        |$outer:{inner |
        |  <li>$inner:{value |
        |    $value$
        |  }$</li>
        |}$
        |
        |<!-- list of refs iteration: List[Map[String, String]] -->
        |<!-- note: does not work -->
      """.stripMargin

    //$outer2:{inner | <li>$inner.name$: $inner.age$</li>}$

    val compiled = ST(template, startDelimiter = '$', endDelimiter = '$')

    val tokenStream = compiled.nativeTemplate.impl.tokens
    val tokens = (0 until tokenStream.range()).map(i => tokenStream.get(i))

    tokens.foreach { token =>
      logger.info(s"token [${token.getType}] [${token.getText}]")
    }

    case class Person(name: String, age: Int)

    val some = Map(
      "user" -> Map("firstName" -> "John", "middleName" -> "Jr", "lastName" -> "Smit"),
      "attachments" -> Seq("a1", "a2"),
      "outer" -> Seq(Seq("00", "01", "02"), Seq("10", "11"), Seq("20")),
      "people" -> Seq(Person("Peter", 10), Person("Liza", 13))
    )

    val none = Map(
      "user" -> Map("firstName" -> "John", "lastName" -> "Smit"),
      "attachments" -> Seq(),
      "outer" -> Seq(),
      "people" -> Seq()
    )

    logger.info(compiled.addAttributes(some).render().get)
    logger.info(compiled.addAttributes(none).render().get)
  }

}
