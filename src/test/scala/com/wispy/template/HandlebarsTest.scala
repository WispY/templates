package com.wispy.template

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.FlatSpec

/** @author Leonid Poliakov */
class HandlebarsTest extends FlatSpec with LazyLogging {

  it should "test" in {
    val template =
      """
        |<!-- path rendering, if-empty-else rendering -->
        |<p>Hello,
        |  {{#with user}}
        |    <span>{{firstName}}</span>
        |    <span>{{#if middleName}}{{middleName}}{{else}}..{{/if}}</span>
        |    <span>{{lastName}}</span>
        |  {{/with}}
        |</p>
        |
        |<!-- list iteration -->
        |{{#if attachments}}
        |  <p>Attachments:</p>
        |  <ul>
        |    {{#each attachments}}<li>{{this}}</li>{{/each}}
        |  </ul>
        |{{/if}}
        |
        |<!-- value switch rendering -->
        |{{#ifvalue status is="ACCEPTED"}}Action accepted{{/ifvalue}}
        |{{#ifvalue status is="DENIED"}}Action denied{{/ifvalue}}
        |{{#ifvalue status is="FAILED"}}Action failed{{/ifvalue}}
        |
        |<!-- list of lists iteration: List[List[String]] -->
        |{{#each outer}}
        |  {{#each this}}
        |    <li>{{this}}</li>
        |  {{/each}}
        |{{/each}}
        |
        |<!-- list of refs iteration: List[Map[String, String]] -->
        |{{#each people}}
        |  <li>{{name}} - {{age}}</li>
        |{{/each}}
      """.stripMargin
  }

}
