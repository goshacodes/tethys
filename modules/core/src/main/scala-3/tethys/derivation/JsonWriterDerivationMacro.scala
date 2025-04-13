package tethys.derivation

import scala.quoted.*
import scala.deriving.Mirror
import tethys.{JsonWriter, JsonObjectWriter, WriterBuilder, JsonConfiguration}
import tethys.writers.tokens.TokenWriter

private[derivation] trait JsonWriterDerivationMacro extends ConfigurationMacroUtils:
  import quotes.reflect.*

  def tokenize[T: Type](
      value: Expr[T],
      builderOpt: Option[Expr[WriterBuilder[T]]],
      out: Expr[TokenWriter],
      legacySumConfig: Option[DiscriminatorConfig] = None
  ): Expr[Unit] = {
    val config =
      Expr.summon[JsonConfiguration].getOrElse('{ JsonConfiguration.default })

    Expr.summon[Mirror.Of[T]] match
      case Some('{ $mirror: Mirror.ProductOf[T] }) =>
        val builder = builderOpt
          .orElse(Expr.summon[WriterBuilder[T]])
          .getOrElse('{ WriterBuilder[T](using $mirror) })

        prepareWriterProductFields[T](builder, config)
          .map(field => write(field.value(value.asTerm), field.tpe, Some(field.label), out))
          .foldLeft('{})((acc, field) => '{ $acc; $field })

      // if this is sum - we generate pattern match
      case Some('{ $mirror: Mirror.SumOf[T] }) =>
        val discriminatorConfig = parseSumConfig[T].discriminator
        if (legacySumConfig.nonEmpty && discriminatorConfig.nonEmpty)
          report.errorAndAbort(
            "Only one discriminator is allowed. Please remove legacy WriterDescription config"
          )
        writePatternMatch(
          value.asTerm,
          None,
          getAllChildren(TypeRepr.of[T]),
          legacySumConfig.orElse(discriminatorConfig),
          out
        )

      case _ =>
        report.errorAndAbort(
          "Derivation is only available for sum and product types"
        )
  }

  private def write(
      value: Term,
      tpe: TypeRepr,
      label: Option[Expr[String]],
      out: Expr[TokenWriter]
  ): Expr[Unit] =
    extractUnionTypes(tpe) match
      case tpe :: Nil =>
        tpe.asType match
          case '[f] =>
            Expr.summon[JsonWriter[f]] match
              case None =>
                report.errorAndAbort(s"JsonWriter[${tpe.show}] is missing")
              case Some(writer) =>
                label match
                  case None =>
                    '{ $writer.write(${ value.asExprOf[f] }, $out) }
                  case Some(label) =>
                    '{ $writer.write($label, ${ value.asExprOf[f] }, $out) }
      case types =>
        writePatternMatch(value, label, types, None, out)

  private def writePatternMatch(
      value: Term,
      label: Option[Expr[String]],
      tpes: List[TypeRepr],
      config: Option[DiscriminatorConfig],
      out: Expr[TokenWriter]
  ): Expr[Unit] =
    value.underlying match
      // reduce nested pattern matches for WriterBuilder
      case Match(selector, cases) =>
        Match(
          selector = selector,
          cases = cases.map { caseDef =>
            val tpe = tpes
              .find(caseDef.rhs.tpe <:< _)
              .getOrElse(
                report.errorAndAbort(
                  s"Not found tpe corresponding to ${caseDef.rhs.tpe} in ${tpes
                      .map(_.show)}"
                )
              )
            CaseDef(
              caseDef.pattern,
              caseDef.guard,
              write(caseDef.rhs, tpe, label, out).asTerm
            )
          }
        ).asExprOf[Unit]
      // write discriminator and then pattern match by type
      case other =>
        val writeDiscriminator = config match {
          case None => '{}
          case Some(DiscriminatorConfig(label, discriminatorTpe, values)) =>
            val term = values match
              // if it is legacy discriminator from WriterDescription - we generate pattern match for it
              case None =>
                Match(
                  selector = value,
                  cases = tpes.map { tpe =>
                    tpe.asType match
                      case '[oneOf] =>
                        CaseDef(
                          pattern = Bind(
                            Symbol.newBind(
                              Symbol.spliceOwner,
                              "v",
                              Flags.EmptyFlags,
                              tpe
                            ),
                            Typed(Wildcard(), TypeTree.of[oneOf])
                          ),
                          guard = None,
                          rhs = Literal(
                            StringConstant(
                              tpe.typeSymbol.name.filterNot(_ == '$')
                            )
                          )
                        )
                  }
                )
              // if it is @selector annotation - we just write discriminator value
              case Some(_) =>
                Select.unique(value, label)

            write(term, discriminatorTpe, Some(Expr(label)), out)
        }

        val writeByTypeMatch = Match(
          selector = value,
          cases = tpes.map { tpe =>
            tpe.asType match
              case '[oneOf] =>
                val bindSymbol =
                  Symbol.newBind(Symbol.spliceOwner, "v", Flags.EmptyFlags, tpe)
                val subTypeValue = Ref(bindSymbol).asExprOf[oneOf]
                CaseDef(
                  pattern = Bind(bindSymbol, Typed(Wildcard(), TypeTree.of[oneOf])),
                  guard = None,
                  rhs = Expr.summon[JsonObjectWriter[oneOf]] match
                    case Some(writer) =>
                      '{ $writer.writeValues($subTypeValue, $out) }.asTerm
                    case None =>
                      tokenize[oneOf](subTypeValue, None, out).asTerm
                )
          }
        ).asExprOf[Unit]
        '{ $writeDiscriminator; $writeByTypeMatch }
