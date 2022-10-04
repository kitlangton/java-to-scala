package course.archive.cohort3.projects

object ImplicitsFromScratch extends App {
  final case class Type(name: String, arguments: List[Type] = List.empty)

  final case class Ident(name: String)

  final case class ImplicitDefinition(
      name: String,
      implicits: List[Type],
      result: Type
  )

  final case class Resolution(
      definition: ImplicitDefinition,
      arguments: List[Resolution] = List.empty
  )

  final case class ImplicitContext(
      map: Map[Type, ImplicitDefinition]
  ) {
    def summon(tpe: Type): Option[Resolution] =
      map.get(tpe) match {
        case Some(definition) =>
          if (definition.implicits.isEmpty) Some(Resolution(definition))
          else {
            val requirements = definition.implicits.map(summon)
            if (requirements.forall(_.isDefined))
              Some(Resolution(definition, requirements.flatten))
            else
              None
          }
      }
  }

  object ImplicitContext {
    def fromDefinitions(definitions: List[ImplicitDefinition]): ImplicitContext =
      ImplicitContext(definitions.map(d => d.result -> d).toMap)
  }

  val definitions = List(
    ImplicitDefinition(
      "int",
      List(Type("String")),
      Type("Int")
    ),
    ImplicitDefinition(
      "string",
      List.empty,
      Type("String")
    )
  )

  val context = ImplicitContext.fromDefinitions(definitions)

  println(pprint.pprintln(context.summon(Type("Int"))))
}
