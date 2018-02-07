package com.wispy.template

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.FlatSpec
import com.wispy.template.templates._
import com.wispy.template.templates.Member._

/** @author Leonid Poliakov */
class TemplatesSpec extends FlatSpec with LazyLogging {

  it should "parse flat freemarker template schema with strings" in {
    assert(
      parseSchema(
        """
          |Hello, ${firstName} ${lastName}!
          |Here is your order id: ${orderId}
          |Class: ${orderClass}
          |Have fun!
        """.stripMargin) ==
        Schema(
          "firstName" -> string,
          "lastName" -> string,
          "orderId" -> string,
          "orderClass" -> string
        )
    )
  }

  it should "parse nested freemarker template schema with refs and strings" in {
    assert(
      parseSchema(
        """
          |Hello, ${user.firstName} ${user.lastName}!
          |Here is your order id: ${order.id}
          |Class: ${order.class}
          |Total: ${order.amounts.total}
          |Tax: ${order.amounts.tax}
          |Have fun!
          |${date}
        """.stripMargin) ==
        Schema(
          "user" -> ref(
            "firstName" -> string,
            "lastName" -> string
          ),
          "order" -> ref(
            "id" -> string,
            "class" -> string,
            "amounts" -> ref(
              "total" -> string,
              "tax" -> string
            )
          ),
          "date" -> string
        )
    )
  }

  it should "fail to parse freemarker template schema that has both 'user' and 'user.lastName'" in {
    val error = intercept[IllegalArgumentException](parseSchema("Hello, ${user} ${user.lastName}!"))
    assert(error.getMessage == "Member 'user' cannot be used as ref 'user.*' and string 'user' at the same time")
  }

  it should "fail to parse freemarker template schema that has both 'user.firstName' and 'user'" in {
    val error = intercept[IllegalArgumentException](parseSchema("Hello, ${user.firstName} ${user}!"))
    assert(error.getMessage == "Member 'user' cannot be used as ref 'user.*' and string 'user' at the same time")
  }

  it should "parse flat freemarker template schema with strings and if-opts" in {
    assert(
      parseSchema(
        """
          |Hello, ${firstName} ${lastName}!
          |Here is your order id: ${orderId}
          |<#if orderClass??>Class: ${orderClass}</#if>
          |Have fun!
        """.stripMargin) ==
        Schema(
          "firstName" -> string,
          "lastName" -> string,
          "orderId" -> string,
          "orderClass" -> opt(string)
        )
    )
  }

  it should "parse nested freemarker template schema with refs, strings and if-opts" in {
    assert(
      parseSchema(
        """
          |Hello, ${user.firstName} ${user.lastName}!
          |Here is your order id: ${order.id}
          |Class: ${order.class}
          |<#if order.amounts??>
          |  Total: ${order.amounts.total}
          |  <#if order.amounts.tax??>Tax: ${order.amounts.tax}</#if>
          |</#if>
          |Have fun!
          |<#if date??>${date}</#if>
        """.stripMargin) ==
        Schema(
          "user" -> ref(
            "firstName" -> string,
            "lastName" -> string
          ),
          "order" -> ref(
            "id" -> string,
            "class" -> string,
            "amounts" -> opt(
              ref(
                "total" -> string,
                "tax" -> opt(string)
              )
            )
          ),
          "date" -> opt(string)
        )
    )
  }

  it should "fail to parse freemarker template schema that has both 'user' and '<#if user.lastName??>'" in {
    val error = intercept[IllegalArgumentException](parseSchema("Hello, ${user} <#if user.lastName??>TEST</#if>!"))
    assert(error.getMessage == "Member 'user' cannot be used as ref 'user.*' and string 'user' at the same time")
  }


  it should "parse flat freemarker template schema with strings and default-opts" in {
    assert(
      parseSchema(
        """
          |Hello, ${firstName} ${lastName}!
          |Here is your order id: ${orderId}
          |Class: ${orderClass!"none"}</#if>
          |Have fun!
        """.stripMargin) ==
        Schema(
          "firstName" -> string,
          "lastName" -> string,
          "orderId" -> string,
          "orderClass" -> opt(string)
        )
    )
  }

  it should "parse nested freemarker template schema with refs, strings and default-opts" in {
    assert(
      parseSchema(
        """
          |Hello, ${user.firstName} ${user.lastName}!
          |Here is your order id: ${order.id}
          |Class: ${order.class}
          |Total: ${order.amounts.total}
          |Tax: ${order.amounts.tax!"0.0"}
          |Have fun!
          |${date!""}
        """.stripMargin) ==
        Schema(
          "user" -> ref(
            "firstName" -> string,
            "lastName" -> string
          ),
          "order" -> ref(
            "id" -> string,
            "class" -> string,
            "amounts" -> ref(
              "total" -> string,
              "tax" -> opt(string)
            )
          ),
          "date" -> opt(string)
        )
    )
  }


  it should "parse nested freemarker template schema with refs, strings, if-opts and default-opts" in {
    assert(
      parseSchema(
        """
          |Hello, ${user.firstName} ${user.lastName}!
          |Here is your order id: ${order.id}
          |Class: ${order.class}
          |<#if order.amounts??>
          |  Total: ${order.amounts.total}
          |  Tax: ${order.amounts.tax!"0.0"}
          |</#if>
          |Have fun!
          |${date!""}
        """.stripMargin) ==
        Schema(
          "user" -> ref(
            "firstName" -> string,
            "lastName" -> string
          ),
          "order" -> ref(
            "id" -> string,
            "class" -> string,
            "amounts" -> opt(
              ref(
                "total" -> string,
                "tax" -> opt(string)
              )
            )
          ),
          "date" -> opt(string)
        )
    )
  }

  case class Model(firstName: String, lastName: String, orderId: Int, category: Option[String])

}
