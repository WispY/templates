package com.wispy.template

import com.wispy.template.regex._

/** Contains freemarker templates schema parsing and validation logic.
  *
  * @author Leonid Poliakov
  */
object freemarker {
  /** Parses the schema from a given freemarker template */
  def parseSchema(template: String): Schema = parseSection(template, Schema.empty, Map())

  /** Maps variable name to the actual path from the schema root */
  type Variables = Map[String, Seq[String]]

  private def parseSection(template: String, root: Schema, variables: Variables): Schema = {
    val sections = template.findSections("""<#list ([^>]+)>""", """</#list>""")
    val borders = sections.flatMap { case (start, end) => Seq(start.start, end.end) }
    val schemaBetweenSections = (Seq(0) ++ borders ++ Seq(template.length))
      .grouped(2)
      .foldLeft(root) { case (schema, Seq(sectionStart, sectionEnd)) =>
        val section = template.substring(sectionStart, sectionEnd)

        // ${user.firstName} or ${user.firstName!"John"}
        val dollarTokens = section.findMatches("""\$\{([^\}]+)\}""")
        // ${user.firstName}
        val variableTokens = dollarTokens.subMatches("""([\d\w.]+)""")
        // <#if user.firstName??>
        val optIfTokens = section.findMatches("""<#if ([^?]+)\?\?>""")
        // ${user.firstName!"John"}
        val optDefaultTokens = dollarTokens.subMatches("""([\d\w.]+)!""")

        val schemaWithVariables = variableTokens.foldLeft(schema)((schema, token) => schema.withVariableToken(token, variables))
        val schemaWithOpts = (optIfTokens ++ optDefaultTokens).foldLeft(schemaWithVariables)((schema, token) => schema.withOptToken(token, variables))
        schemaWithOpts
      }

    val schemaWithSections = sections.foldLeft(schemaBetweenSections) { case (schema, (start, end)) =>
      val section = template.substring(start.end, end.start)
      val (listToken, elementVariable) = "([^ ]+) as ([^ ]+)".r
        .findFirstMatchIn(start.group(1))
        .map(m => (m.group(1), m.group(2)))
        .getOrElse(throw new IllegalArgumentException(s"Failed to extract variable names from the #list statement '${start.group(0)}"))
      // recursive
      parseSection(section, schema.withListToken(listToken, variables), variables + (elementVariable -> tokenToPath(listToken, variables)))
    }

    schemaWithSections
  }

  /** Converts the token to path replacing the beginning with a mapped value, if such variable was defined */
  private def tokenToPath(token: String, variables: Variables): Seq[String] = {
    val path = token.split('.').toSeq
    variables
      .get(path.head)
      .map(replacement => replacement ++ path.drop(1))
      .getOrElse(path)
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
    def withVariableToken(token: String, variables: Variables): Schema = withVariablePath(tokenToPath(token, variables), "")

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

        case Some(Member.list(Member.string)) if single =>
          this // no schema changes required
        case Some(_: Member.list) if single =>
          throw new IllegalArgumentException(s"Member '$subPrefix' cannot be used as string '$subPrefix' and list '#list $subPrefix' at the same time")
        case Some(Member.list(Member.string)) if !single =>
          Schema(members + (root -> Member.list(Member.ref(Schema.empty.withVariablePath(subPath, subPrefix)))))
        case Some(Member.list(ref: Member.ref)) if !single =>
          Schema(members + (root -> Member.list(Member.ref(ref.schema.withVariablePath(subPath, subPrefix)))))
      }
    }

    /** Returns a new schema with the existing member marked as optional or a new optional member */
    def withOptToken(token: String, variables: Variables): Schema = withOptPath(tokenToPath(token, variables), "")

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

    /** Returns a new schema with the new list member */
    def withListToken(token: String, variables: Variables): Schema = withListPath(tokenToPath(token, variables), "")

    /** Returns a new schema with the new list member */
    private def withListPath(path: Seq[String], prefix: String): Schema = {
      val root = path.head
      val single = path.length == 1
      val subPath = path.drop(1)
      val subPrefix = if (prefix.isEmpty) root else s"$prefix.$root"
      members.get(root) match {
        case None if single =>
          Schema(members + (root -> Member.list(Member.string)))
        case None if !single =>
          Schema(members + (root -> Member.ref(Schema.empty.withListPath(subPath, subPrefix))))

        case Some(Member.string) if single =>
          throw new IllegalArgumentException(s"Member '$subPrefix' cannot be used as list '#list $subPrefix' and string '$subPrefix' at the same time")
        case Some(Member.string) if !single =>
          throw new IllegalArgumentException(s"Member '$subPrefix' cannot be used as ref '$subPrefix.*' and string '$subPrefix' at the same time")

        case Some(_: Member.ref) if single =>
          throw new IllegalArgumentException(s"Member '$subPrefix' cannot be used as list '#list $subPrefix' and ref '$subPrefix.*' at the same time")
        case Some(ref: Member.ref) if !single =>
          Schema(members + (root -> Member.ref(ref.schema.withListPath(subPath, subPrefix))))

        case Some(_: Member.option) if single =>
          Schema(members + (root -> Member.option(Member.list(Member.string))))
        case Some(Member.option(ref: Member.ref)) if !single =>
          Schema(members + (root -> Member.option(Member.ref(ref.schema.withListPath(subPath, subPrefix)))))

        case Some(Member.list(Member.string)) if single =>
          this // no schema changes required
        case Some(Member.list(ref: Member.ref)) if !single =>
          Schema(members + (root -> Member.list(Member.ref(ref.schema.withListPath(subPath, subPrefix)))))
      }
    }
  }

  object Schema {
    def empty: Schema = Schema(Map[String, Member]())

    def apply(members: (String, Member)*): Schema = Schema(Map(members: _*))
  }

}
