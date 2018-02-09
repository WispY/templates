package com.wispy.template

import com.typesafe.scalalogging.LazyLogging
import com.wispy.template.mustache.Member._
import com.wispy.template.mustache._
import org.scalatest.FlatSpec

/** @author Leonid Poliakov */
class MustacheSpec extends FlatSpec with LazyLogging {

  it should "pass schema validation for template with leafs and simple list" in {
    val template =
      """
        |Hello, my name is {{name}}.
        |I am from {{hometown}}.
        |I have {{kidsSize}} kids:
        |{{#kids}}
        |  {{name}} is {{age}}
        |{{/kids}}
      """.stripMargin
    val schema = Schema(
      "name" -> leaf,
      "hometown" -> leaf,
      "kidsSize" -> leaf,
      "kids" -> ref(
        "name" -> leaf,
        "age" -> leaf
      )
    )
    assert(validateSchema(template, schema) == Seq())
  }

  it should "pass schema validation for template with sub-lists" in {
    val template =
      """
        |{{#users}}
        |  User: {{name}}
        |  Roles:
        |  {{#roles}}
        |    {{name}} - {{active}}
        |  {{/roles}}
        |{{/users}}
      """.stripMargin
    val schema = Schema(
      "users" -> ref(
        "name" -> leaf,
        "roles" -> ref("name" -> leaf, "active" -> leaf)
      )
    )
    assert(validateSchema(template, schema) == Seq())
  }

  it should "pass schema validation for template with sub-refs" in {
    val template =
      """
        |{{#user}}
        |  User: {{name}}
        |  Address: {{#address}}{{street}}, {{city}}, {{state}} {{zip}}{{/address}}
        |{{/user}}
      """.stripMargin
    val schema = Schema(
      "user" -> ref(
        "name" -> leaf,
        "address" -> ref("street" -> leaf, "city" -> leaf, "state" -> leaf, "zip" -> leaf)
      )
    )
    assert(validateSchema(template, schema) == Seq())
  }

  it should "pass schema validation for template with lookbacks ans sub-refs" in {
    val template =
      """
        |{{#users}}
        |  User: {{name}}
        |  {{#scores}}
        |    Score: {{value}}
        |  {{/scores}}
        |{{/users}}
      """.stripMargin
    val schema = Schema(
      "users" -> ref("name" -> leaf),
      "scores" -> ref("value" -> leaf)
    )
    assert(validateSchema(template, schema) == Seq())
  }

  it should "pass schema validation for lists of leaves" in {
    assert(validateSchema("{{#users}}{{.}}{{/users}}", Schema("users" -> ref.empty)) == Seq())
  }

  it should "fail schema validation when schema is missing a leaf" in {
    assert(validateSchema("{{name}}", Schema.empty) == Seq("template refers to 'name', but schema is missing such leaf"))
  }

  it should "fail schema validation when schema is missing a sub-leaf" in {
    assert(validateSchema("{{#user}}{{name}}{{/user}}", Schema("user" -> ref("age" -> leaf))) == Seq("template refers to 'user.name', but schema is missing such leaf"))
  }

  it should "fail schema validation for partials" in {
    assert(validateSchema("{{> user}}", Schema("user" -> leaf)) == Seq("rendering Partial is not supported"))
  }

  it should "fail schema validation for closest lookback with missing sub-ref" in {
    val template =
      """
        |{{#users}}
        |  User: {{name}}
        |  {{#scores}}
        |    {{#lookback}}
        |      Value: {{value}}
        |    {{/lookback}}
        |  {{/scores}}
        |{{/users}}
      """.stripMargin
    val schema = Schema(
      "users" -> ref(
        "name" -> leaf,
        "scores" -> ref("ignored" -> leaf),
        "lookback" -> ref("notvalue" -> leaf) // closest lookback, but does not have "value"
      ),
      "lookback" -> ref("value" -> leaf) // furthest lookback, has "value"
    )
    assert(validateSchema(template, schema) == Seq("template refers to 'users.lookback.value', but schema is missing such leaf"))
  }


  it should "pass schema validation for unescaped tag" in {
    assert(validateSchema("{{{name}}}", Schema("name" -> leaf)) == Seq())
  }

  it should "pass schema validation for inverted section" in {
    assert(validateSchema("{{^user}}empty{{/user}}", Schema("user" -> leaf)) == Seq())
  }

  it should "pass schema validation for inverted section with sub-leaf" in {
    assert(validateSchema("{{^user}}{{emptyMessage}}{{/user}}", Schema("user" -> leaf, "emptyMessage" -> leaf)) == Seq())
  }

  it should "pass schema validation for comments" in {
    assert(validateSchema("{{! ignore me }}", Schema.empty) == Seq())
  }

}
