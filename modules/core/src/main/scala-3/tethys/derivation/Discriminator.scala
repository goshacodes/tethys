package tethys.derivation

private[tethys]
object Discriminator:

  inline def getLabel[Type, Discriminator]: String =
    ${ DiscriminatorMacro.getLabel[Type, Discriminator] }

  inline def getValue[Type, SubType, Discriminator](label: String): Any =
    ${ DiscriminatorMacro.getValue[Type, SubType, Discriminator]('{ label }) }

private[derivation]
object DiscriminatorMacro:
  import scala.quoted.*

  def getLabel[T: Type, D: Type](using quotes: Quotes): Expr[String] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val selectorTpe = TypeRepr.of[D]
    val symbol = tpe.typeSymbol.fieldMembers
      .find(tpe.memberType(_) =:= selectorTpe)
      .getOrElse(report.errorAndAbort(s"Selector of type ${selectorTpe.show(using Printer.TypeReprShortCode)} not found in ${tpe.show(using Printer.TypeReprShortCode)}"))

      tpe.typeSymbol.children
        .find(child => child.caseFields.contains(symbol.overridingSymbol(child)))
        .foreach { child =>
          report.errorAndAbort(s"Overriding discriminator field '${symbol.name}' in ${child.typeRef.show(using Printer.TypeReprShortCode)} is prohibited")
        }

    Expr(symbol.name)


  def getValue[T: Type, ST: Type,  D: Type](label: Expr[String])(using quotes: Quotes): Expr[Any] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val selectorTpe = TypeRepr.of[D]
    val symbol = tpe.typeSymbol.fieldMembers
      .find(tpe.memberType(_) =:= selectorTpe)
      .getOrElse(report.errorAndAbort(s"Selector of type ${selectorTpe.show(using Printer.TypeReprShortCode)} not found in ${tpe.show(using Printer.TypeReprShortCode)}"))

    Select(stub[ST].asTerm, symbol).asExprOf[Any]


  def stub[T: Type](using quotes: Quotes): Expr[T] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val symbol = tpe.typeSymbol
    val constructorFieldsFilledWithNulls: List[List[Term]] =
      symbol.primaryConstructor.paramSymss
        .filterNot(_.exists(_.isType))
        .map(_.map(_.typeRef.widen match {
          case t@AppliedType(inner, applied) =>
            Select.unique('{ null }.asTerm, "asInstanceOf").appliedToTypes(List(inner.appliedTo(tpe.typeArgs)))
          case other =>
            Select.unique('{ null }.asTerm, "asInstanceOf").appliedToTypes(List(other))
        }))

    New(TypeTree.ref(symbol)).select(symbol.primaryConstructor)
      .appliedToTypes(symbol.typeRef.typeArgs.map(_ => TypeRepr.of[Null]))
      .appliedToArgss(constructorFieldsFilledWithNulls)
      .asExprOf[T]