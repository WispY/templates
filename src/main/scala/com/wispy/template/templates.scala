package com.wispy.template

import scala.reflect.runtime.universe._

/** Contains freemarker templates schema parsing and validation logic.
  *
  * @author Leonid Poliakov
  */
object templates {
  /** Parses the schema from a given freemarker template */
  def parseSchema(template: String): Schema = {
    // ${user.firstName} or ${user.firstName!"John"}
    val dollarTokens = findAll("""\$\{([^\}]+)\}""", template)
    // ${user.firstName}
    val variableTokens = subMatch("""([\d\w.]+)""", dollarTokens)
    // <#if user.firstName??>
    val optIfTokens = findAll("""<#if ([^?]+)\?\?>""", template)
    // ${user.firstName!"John"}
    val optDefaultTokens = subMatch("""([\d\w.]+)!""", dollarTokens)

    val variableSchema = variableTokens.foldLeft(Schema.empty)((schema, token) => schema.withVariableToken(token))
    val optIfSchema = optIfTokens.foldLeft(variableSchema)((schema, token) => schema.withOptToken(token))
    val optDefaultSchema = optDefaultTokens.foldLeft(optIfSchema)((schema, token) => schema.withOptToken(token))
    optDefaultSchema
  }

  /** Finds all regex (group 1) matches for a given content string */
  private def findAll(regex: String, content: String): Seq[String] = regex.r.findAllMatchIn(content).toSeq.map(m => m.group(1))

  /** Filters and fetches first regex (group 1) matches for a given seq of strings */
  private def subMatch(regex: String, content: Seq[String]): Seq[String] = {
    val expression = regex.r
    content.flatMap(s => expression.findFirstMatchIn(s).map(m => m.group(1)))
  }

  /** Represents the schema node */
  sealed trait Member

  object Member {

    case object string extends Member

    case class option(member: Member) extends Member

    case class list(member: Member) extends Member

    case class ref(schema: Schema) extends Member

    object ref {
      def apply(members: (String, Member)*): ref = ref(Schema(members: _*))
    }

  }

  /** Represents the schema of the template */
  case class Schema(members: Map[String, Member]) {
    /** Returns a new schema with an additional token "some.path.to.value" if that path does not already exists */
    def withVariableToken(token: String): Schema = withVariablePath(token.split('.').toSeq, "")

    /** Returns a new schema with an additional path "some.path.to.value" if that path does not already exists */
    private def withVariablePath(path: Seq[String], prefix: String): Schema = {
      val root = path.head
      val single = path.length == 1
      val subPath = path.drop(1)
      val subPrefix = if (prefix.isEmpty) root else s"$prefix.$root"
      members.get(root) match {
        case None if single =>
          Schema(members + (root -> Member.string))
        case None if !single =>
          Schema(members + (root -> Member.ref(Schema.empty.withVariablePath(subPath, subPrefix))))

        case Some(Member.string) if single =>
          this // no schema changes required
        case Some(Member.string) if !single =>
          throw new IllegalArgumentException(s"Member '$subPrefix' cannot be used as ref '$subPrefix.*' and string '$subPrefix' at the same time")

        case Some(_: Member.ref) if single =>
          throw new IllegalArgumentException(s"Member '$subPrefix' cannot be used as ref '$subPrefix.*' and string '$subPrefix' at the same time")
        case Some(ref: Member.ref) if !single =>
          Schema(members + (root -> Member.ref(ref.schema.withVariablePath(subPath, subPrefix))))
      }
    }

    /** Returns a new schema with the existing member marked as optional or a new optional member */
    def withOptToken(token: String): Schema = withOptPath(token.split('.').toSeq, "")

    /** Returns a new schema with the existing member marked as optional or a new optional member */
    private def withOptPath(path: Seq[String], prefix: String): Schema = {
      val root = path.head
      val single = path.length == 1
      val subPath = path.drop(1)
      val subPrefix = if (prefix.isEmpty) root else s"$prefix.$root"
      members.get(root) match {
        case None if single =>
          Schema(members + (root -> Member.option(Member.string)))
        case None if !single =>
          Schema(members + (root -> Member.ref(Schema.empty.withOptPath(subPath, subPrefix))))

        case Some(Member.string) if single =>
          Schema(members + (root -> Member.option(Member.string)))
        case Some(Member.string) if !single =>
          throw new IllegalArgumentException(s"Member '$subPrefix' cannot be used as ref '$subPrefix.*' and string '$subPrefix' at the same time")

        case Some(ref: Member.ref) if single =>
          Schema(members + (root -> Member.option(ref)))
        case Some(ref: Member.ref) if !single =>
          Schema(members + (root -> Member.ref(ref.schema.withOptPath(subPath, subPrefix))))

        case Some(Member.option(Member.string)) =>
          this // no schema changes required
        case Some(_: Member.option) if single =>
          this // no schema changes required
        case Some(Member.option(ref: Member.ref)) if !single =>
          Schema(members + (root -> Member.option(Member.ref(ref.schema.withOptPath(subPath, subPrefix)))))
      }
    }
  }

  object Schema {
    def empty: Schema = Schema(Map[String, Member]())

    def apply(members: (String, Member)*): Schema = Schema(Map(members: _*))
  }

  //val loader = new StringTemplateLoader()
  //val config = new Configuration(VERSION_2_3_23)
  //config.setTemplateLoader(loader)
  //config.setLogTemplateExceptions(false)
  //loader.putTemplate("template", template)
  //val writer = new StringWriter()
  //config.getTemplate("template").process(mapAsJavaMap(Map("firstName" -> "John", "lastName" -> "Smit", "orderId" -> 42)), writer)
  ////    template.process(mapAsJavaMap(Map("firstName" -> "John", "lastName" -> "Smit", "orderId" -> 42, "category" -> "fruit")), writer)
  //// template.process(Model("John", "Smit", 42, Some("fruit")), writer)
  //logger.info(writer.toString)

}
