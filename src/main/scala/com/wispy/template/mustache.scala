package com.wispy.template

import org.fusesource.scalate.mustache._

/** Contains mustache templates schema parsing and validation logic.
  *
  * @author Leonid Poliakov
  */
object mustache {
  private val parser = new MustacheParser()

  /** Describes the full schema and string path to the current location, used for loopbacks */
  private case class Lookup(schemas: Seq[Schema], path: Seq[String]) {
    def parent: Option[Lookup] = if (path.isEmpty) None else Some(Lookup(schemas.dropRight(1), path.dropRight(1)))

    def member(name: String): Option[Member] = schemas.last.members.get(name)

    def sub(subSchema: Schema, subPath: String): Lookup = Lookup(schemas ++ Seq(subSchema), path ++ Seq(subPath))

    def printedPathTo(name: String): String = (path ++ Seq(name)).mkString(".")
  }

  /** Validates the mustache template against the given schema, raising a list of errors if any found */
  def validateSchema(template: String, schema: Schema): Seq[String] = {
    val statements = parser.parse(template)
    statements.flatMap(s => compare(s, Lookup(Seq(schema), Seq())))
  }

  /** Checks if statement is present in current lookup or the lookups above */
  private def compare(statement: Statement, lookup: Lookup): Seq[String] = statement match {
    case variable: Variable =>
      val name = variable.name.value
      val printedPath = lookup.printedPathTo(name)
      lookup.member(name) match {
        case _ if name == "." => Seq()
        case Some(_) => Seq()
        case None => lookback(name, lookup)
          .map(l => compare(statement, l))
          .getOrElse(Seq(s"template refers to '$printedPath', but schema is missing such leaf"))
      }
    case section: Section =>
      val name = section.name.value
      val printedPath = lookup.printedPathTo(name)
      lookup.member(name) match {
        case Some(Member.leaf) => Seq(s"template uses '$printedPath' as a section, but schema contains it as a leaf")
        case Some(Member.ref(subSchema)) => section.body.flatMap(subStatement => compare(subStatement, lookup.sub(subSchema, name)))
        case None => lookback(name, lookup)
          .map(l => compare(statement, l))
          .getOrElse(Seq(s"template uses '$printedPath' as a section, but schema is mussing such ref"))
      }
    case inverted: InvertSection =>
      val name = inverted.name.value
      val printedPath = lookup.printedPathTo(name)
      lookup.member(name) match {
        case Some(_) => inverted.body.flatMap(subStatement => compare(subStatement, lookup))
        case None => lookback(name, lookup)
          .map(l => compare(statement, l))
          .getOrElse(Seq(s"template refers to '$printedPath', but schema is missing such member"))
      }
    case _: Text => Seq()
    case _: Comment => Seq()
    case other => Seq(s"rendering ${other.getClass.getSimpleName} is not supported")
  }

  /** Attempts to find Lookup that contains the given tag by going backwards through the path */
  private def lookback(name: String, lookup: Lookup): Option[Lookup] = {
    if (lookup.schemas.last.members.contains(name)) Some(lookup) else lookup.parent.flatMap(parent => lookback(name, parent))
  }

  /** Represents the schema node */
  sealed trait Member

  object Member {

    /** Represents the end of the path variable that is just printed during rendering */
    case object leaf extends Member

    /** Represents an embedding wrapper around other values, can be AnyRef, Option or List */
    case class ref(schema: Schema) extends Member

    object ref {
      /** Convenient method to build a ref from a member key-value pairs */
      def apply(members: (String, Member)*): ref = ref(Schema(members: _*))

      /** Builds a ref without any members in schema, use this for refs that wrap the {{.}} tag */
      def empty: ref = ref(Schema.empty)
    }

  }

  /** Represents the schema of the template */
  case class Schema(members: Map[String, Member]) {
  }

  object Schema {
    /** Convenient method to build a schema from a member key-value pairs */
    def apply(members: (String, Member)*): Schema = Schema(Map(members: _*))

    /** Builds an empty schema without members */
    def empty: Schema = Schema(Map[String, Member]())
  }

}
