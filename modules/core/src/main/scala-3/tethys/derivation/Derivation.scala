package tethys.derivation

import tethys.writers.tokens.TokenWriter
import tethys.readers.{FieldName, ReaderError}
import tethys.commons.{Token, TokenNode}
import tethys.readers.tokens.{QueueIterator, TokenIterator}
import tethys.{JsonConfig, JsonObjectWriter, JsonReader, JsonWriter, ReaderBuilder, WriterBuilder}

import scala.Tuple2
import scala.annotation.tailrec
import scala.compiletime.{constValueTuple, summonInline}
import scala.quoted.*
import scala.collection.mutable
import scala.deriving.Mirror

private[tethys]
object Derivation:

  inline def deriveJsonWriterForProduct[T](inline config: WriterBuilder[T]): JsonObjectWriter[T] =
    ${ DerivationMacro.deriveJsonWriterForProduct[T]('{config})}

  inline def deriveJsonWriterForSum[T](inline config: JsonConfig[T]): JsonObjectWriter[T] =
    ${ DerivationMacro.deriveJsonWriterForSum[T]('{config}) }

  inline def deriveJsonReaderForProduct[T](inline config: ReaderBuilder[T]): JsonReader[T] =
    ${ DerivationMacro.deriveJsonReaderForProduct[T]('{config})}

  inline def deriveJsonReaderForSum[T](inline config: JsonConfig[T]): JsonReader[T] =
    ${ DerivationMacro.deriveJsonReaderForSum[T]('{config})}

object DerivationMacro:
  def deriveJsonWriterForProduct[T: Type](config: Expr[WriterBuilder[T]])(using quotes: Quotes): Expr[JsonObjectWriter[T]] =
    new DerivationMacro(quotes).deriveJsonWriterForProduct[T](config)

  def deriveJsonWriterForSum[T: Type](config: Expr[JsonConfig[T]])(using quotes: Quotes): Expr[JsonObjectWriter[T]] =
    new DerivationMacro(quotes).deriveJsonWriterForSum[T](config)

  def deriveJsonReaderForProduct[T: Type](config: Expr[ReaderBuilder[T]])(using quotes: Quotes): Expr[JsonReader[T]] =
    new DerivationMacro(quotes).deriveJsonReaderForProduct[T](config)

  def deriveJsonReaderForSum[T: Type](config: Expr[JsonConfig[T]])(using quotes: Quotes): Expr[JsonReader[T]] =
    new DerivationMacro(quotes).deriveJsonReaderForSum(config)

private[derivation]
class DerivationMacro(val quotes: Quotes) extends ConfigurationMacroUtils:
  import quotes.reflect.*

  def deriveJsonWriterForProduct[T: Type](config: Expr[WriterBuilder[T]]): Expr[JsonObjectWriter[T]] =
    val fields = prepareWriterProductFields(config)
    Block(
      deriveMissingWriters[T](fields.map(_.tpe)),
      '{
        new JsonObjectWriter[T]:
          override def writeValues(value: T, tokenWriter: TokenWriter): Unit =
            ${
              Expr.block(
                fields.map { field => field.tpe.asType match
                  case '[f] =>
                    '{
                      searchJsonWriter[f]
                        .write(${ field.label }, ${ field.value('{ value }.asTerm).asExprOf[f] }, tokenWriter) }
                },
                '{}
              )
            }
      }.asTerm
    ).asExprOf[JsonObjectWriter[T]]

  def deriveJsonWriterForSum[T: Type](config: Expr[JsonConfig[T]]): Expr[JsonObjectWriter[T]] =
    val tpe = TypeRepr.of[T]
    val parsedConfig = parseSumConfig(config)
    val types = getAllChildren(tpe)
    Block(
      deriveMissingWritersForSum[T](types),
    '{
      new JsonObjectWriter[T]:
        override def writeValues(value: T, tokenWriter: TokenWriter): Unit =
          $ {
            parsedConfig.discriminator.fold('{}) { case DiscriminatorConfig(label, tpe, discriminators) =>
              tpe.asType match
                case '[discriminatorType] =>
                  '{
                    searchJsonWriter[discriminatorType].write(
                      name = ${ Expr(label) },
                      value = ${ Select.unique('{ value }.asTerm, label).asExprOf[discriminatorType] },
                      tokenWriter = tokenWriter
                    )
                  }
            }
          }
          ${ matchByTypeAndWrite(
            term = '{ value }.asTerm,
            types = types,
            write = (ref, tpe) => tpe.asType match
              case '[t] =>
                '{ searchJsonObjectWriter[t].writeValues(${ref.asExprOf[t]}, tokenWriter) }
          )
          }
    }.asTerm
    ).asExprOf[JsonObjectWriter[T]]

  private def deriveMissingWritersForSum[T: Type](types: List[TypeRepr]): List[ValDef] =
    types.zipWithIndex
      .flatMap { case (tpe, idx) =>
        tpe.asType match
          case '[t] =>
            val symbol = Symbol.newVal(
              Symbol.spliceOwner,
              s"given_jsonWriter_$idx",
              TypeRepr.of[JsonObjectWriter[t]],
              Flags.Given,
              Symbol.noSymbol
            )
            Option.when(lookupOpt[JsonObjectWriter[t]].isEmpty)(
              ValDef(symbol, Some('{ JsonWriter.derived[t](using ${lookup[Mirror.Of[t]]}) }.asTerm))
            )
      }

  private def deriveMissingWriters[T: Type](tpes: List[TypeRepr]): List[ValDef] =
    distinct(tpes).zipWithIndex
      .flatMap { (tpe, idx) =>
        tpe.asType match
          case '[t] =>
            val symbol = Symbol.newVal(
                Symbol.spliceOwner,
                s"given_jsonWriter_$idx",
                TypeRepr.of[JsonWriter[t]],
                Flags.Given,
                Symbol.noSymbol
              )
            tpe match
              case or: OrType =>
                Option.when(lookupOpt[JsonWriter[t]].isEmpty)(
                  ValDef(
                    symbol,
                    Some(deriveOrTypeJsonWriter[t].asTerm)
                  )
                )
              case _ =>
                Option.when(lookupOpt[JsonWriter[t]].isEmpty)(
                  ValDef(
                    symbol,
                    Some('{JsonObjectWriter.derived[t](using ${lookup[Mirror.Of[t]]})}.asTerm)
                  )
                )
      }

  private def deriveOrTypeJsonWriter[T: Type]: Expr[JsonWriter[T]] =
    def collectTypes(tpe: TypeRepr, acc: List[TypeRepr] = Nil): List[TypeRepr] =
      tpe match
        case OrType(left, right) => collectTypes(left, Nil) ::: acc ::: collectTypes(right, Nil)
        case other => other :: acc

    val types = collectTypes(TypeRepr.of[T])
    val term = Block(
      deriveMissingWriters[T](types),
      '{
        new JsonWriter[T]:
          def write(value: T, tokenWriter: TokenWriter): Unit =
            ${
              matchByTypeAndWrite(
                term = '{ value }.asTerm,
                types = types,
                (ref, tpe) => tpe.asType match
                  case '[t] =>
                    '{ ${ lookup[JsonWriter[t]] }.write(${ref.asExprOf[t]}, tokenWriter) }
              )
            }
      }.asTerm
    )
    term.asExprOf[JsonWriter[T]]



  private def matchByTypeAndWrite(term: Term,
                                  types: List[TypeRepr],
                                  write: (Ref, TypeRepr) => Expr[Unit]): Expr[Unit] =
    Match(
      term,
      types.map { tpe =>
        tpe.asType match
          case '[t] =>
            val valDef = ValDef(
              Symbol.newVal(Symbol.spliceOwner, "value", tpe, Flags.EmptyFlags, Symbol.noSymbol),
              Some(Typed(term, TypeTree.of[t]))
            )
            CaseDef(
              pattern = Bind(valDef.symbol, Typed(Wildcard(), TypeTree.of[t])),
              guard = None,
              rhs = write(Ref(valDef.symbol), tpe).asTerm
            )
      }
    ).asExprOf[Unit]

  def deriveJsonReaderForProduct[T: Type](config: Expr[ReaderBuilder[T]]): Expr[JsonReader[T]] =
    val tpe = TypeRepr.of[T]
    val (fields, isStrict) = prepareReaderProductFields[T](config)
    val labelsToIndices = fields.map(field => field.name -> field.idx).toMap
    val existingLabels = fields.map(_.name).toSet
    val defaults: Map[String, Expr[Any]] = fields.flatMap(_.defaults(existingLabels)).distinctBy(_._1).toMap
    val requiredLabels = fields.flatMap(_.requiredLabels(existingLabels)).toSet -- defaults.keys
    val fieldsWithoutReader = fields.collect { case (field: ReaderField.Extracted) if field.reader => field.name }
    val fieldNamesWithTypes = fields.flatMap(_.readerTypes(existingLabels)).distinctBy(_._1)

    case class ExpectedField(name: String)

    val expectedFields = fields.flatMap {
      case field: ReaderField.Basic =>
        List((field.name, field.extractor.map(_._1).getOrElse(field.tpe), defaults.get(field.name)))
      case field: ReaderField.Extracted =>
        field.extractors.map((name, tpe) => (name, tpe, defaults.get(name)))
    }.distinctBy(_._1)


    val sortedFields: List[(String, TypeRepr, Option[(TypeRepr, Term)], List[(String, TypeRepr)], Boolean)] =
      fields.map {
        case field: ReaderField.Basic =>
          (field.name, field.tpe, field.extractor, Nil, false)
        case field: ReaderField.Extracted =>
          (field.name, field.tpe, Some((field.tpe, field.lambda)), field.extractors, field.reader)
      }

    case class FieldRefs(name: String, ref: Ref, initialized: Ref, default: Option[Expr[Any]])

    val (defaultStats, defaultsByName) = expectedFields.map { (name, tpe, defaultOpt) =>
      val symbol = Symbol.newVal(Symbol.spliceOwner, s"${name}DefaultVar", tpe, Flags.EmptyFlags, Symbol.noSymbol)
      val default = defaultOpt.getOrElse {
        tpe.asType match
          case '[Boolean] => '{false}
          case '[Short] => '{0}
          case '[Int] => '{0}
          case '[Long] => '{0}
          case '[Float] => '{0f}
          case '[Double] => '{0.0}
          case '[Byte] => '{0}
          case '[Char] => '{0}
          case _ => '{null}
      }.asTerm
      val valDef = ValDef(symbol, Some(default))
      (valDef, (name, Ref(valDef.symbol)))
    }.unzip

    val (stats, refs) = expectedFields.map { (name, tpe, defaultOpt) =>
      val symbol = Symbol.newVal(Symbol.spliceOwner, s"${name}Var", tpe, Flags.Mutable, Symbol.noSymbol)
      val initSymbol = Symbol.newVal(Symbol.spliceOwner, s"${name}Init", TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)
      val stat = ValDef(symbol, Some(defaultsByName.find(_._1 == name).get._2))
      val initStat = ValDef(initSymbol, Some('{false}.asTerm))
      (List(stat, initStat), FieldRefs(name, Ref(stat.symbol), Ref(initStat.symbol), defaultOpt))
    }.unzip

    val fieldRefs = refs.map(it => it.name -> it).toMap

    def initialize(name: String, value: Term): Expr[Unit] =
      val ref = fieldRefs(name).ref
      val init = fieldRefs(name).initialized
      '{
        ${ Assign(ref, value).asExprOf[Unit] }
        ${ Assign(init, '{ true }.asTerm).asExprOf[Unit] }
      }

    def failIfNotInitialized(fieldName: Expr[FieldName]): Expr[Unit] =
      refs.filterNot(_.default.nonEmpty) match
        case refs @ head :: tail =>
          val boolExpr = tail.foldLeft('{!${head.initialized.asExprOf[Boolean]}}) { (acc, el) =>
            '{${acc} || !${el.initialized.asExprOf[Boolean]}}
          }
          '{
            if {$boolExpr} then
              val uninitializedFields = new scala.collection.mutable.ArrayBuffer[String](${ Expr(refs.size) })
              ${
                Expr.block(
                  refs.map { ref =>
                    '{
                      if !${ref.initialized.asExprOf[Boolean]} then
                        uninitializedFields += ${Expr(ref.name)}
                    }
                  },
                '{}
                )
              }
              ReaderError.wrongJson("Can not extract fields from json: " + uninitializedFields.mkString(", "))(${fieldName})
          }

        case Nil =>
          '{}

    def newInstance(args: List[Term]): Expr[T] =
      New(TypeTree.of[T])
        .select(tpe.classSymbol.get.primaryConstructor)
        .appliedToArgs(args)
        .asExprOf[T]


    val term = Block(
      deriveMissingReaders(fieldNamesWithTypes.map(_._2)) ++ defaultStats,
      '{
        new JsonReader[T]:
          given JsonReader[T] = this

          def read(it: TokenIterator)(using fieldName: FieldName) =
            if !it.currentToken().isObjectStart then
              ReaderError.wrongJson("Expected object start but found: " + it.currentToken().toString)
            else
              it.nextToken()

              ${
                Block(
                  stats.flatten,
                  '{
                    while (!it.currentToken().isObjectEnd)
                      val jsonName = it.fieldName()
                      it.nextToken()
                      val currentIt = it.collectExpression()

                      given FieldName = fieldName.appendFieldName(jsonName)

                      ${
                        Match(
                          '{ jsonName }.asTerm,
                          fieldNamesWithTypes.map { (name, tpe) =>
                            tpe.asType match {
                              case '[t] =>
                                CaseDef(
                                  Literal(StringConstant(name)),
                                  None,
                                  initialize(name, '{ ${lookup[JsonReader[t]]}.read(currentIt.copy()) }.asTerm).asTerm
                                )
                            }
                          } :+
                            CaseDef(
                              Wildcard(),
                              None,
                              '{it.skipExpression(); ()}.asTerm
                            )
                        ).asExprOf[Unit]
                      }

                    it.nextToken()

                    ${ failIfNotInitialized('{fieldName}) }

                    ${
                      newInstance(
                        sortedFields.map { (name, tpe, fun, deps, _) =>
                          tpe.asType match {
                            case '[typ] =>
                              (fun, deps) match {
                                case (Some((fromTpe, fun)), Nil) =>
                                  fromTpe.asType match
                                    case '[fromTyp] =>
                                      '{ ${ fun.asExprOf[fromTyp => typ] }.apply(${ fieldRefs(name).ref.asExprOf[fromTyp] }) }

                                case (None, Nil) =>
                                  fieldRefs(name).ref.asExprOf[typ]

                                case (Some((_, fun)), List(depName)) =>
                                  '{ ${ fun.asExprOf[Any => Any] }.apply(${ fieldRefs(name).ref.asExprOf[Any] }) }

                                case (Some(fun), deps) =>
                                  report.errorAndAbort("Internal error, function not provided for dependency")

                                case (None, _) =>
                                  report.errorAndAbort("Internal error, function not provided for dependency")
                              }
                          }
                        }.map(_.asTerm)

                        )
                    }

                  }.asTerm
                ).asExprOf[T]
              }

        }.asTerm
    )

    println(term.show(using Printer.TreeAnsiCode))
    term.asExprOf[JsonReader[T]]

  def deriveJsonReaderForSum[T: Type](config: Expr[JsonConfig[T]]): Expr[JsonReader[T]] =
    val tpe = TypeRepr.of[T]
    val parsed = parseSumConfig(config)
    val children = getAllChildren(tpe)
    parsed.discriminator match
      case Some(DiscriminatorConfig(label, tpe, discriminators)) => tpe.asType match
        case '[discriminator]  =>
          val (discriminatorStats, discriminatorRefs) = discriminators.zipWithIndex.map((term, idx) =>
            val stat = ValDef(
              Symbol.newVal(Symbol.spliceOwner, s"Discriminator_$idx", term.tpe, Flags.Private, Symbol.noSymbol),
              Some(term)
            )
            (stat, Ref(stat.symbol))
          ).unzip
          Block(
            deriveMissingReaders(children) ++ discriminatorStats,
            '{
              JsonReader.builder
                .addField[discriminator](
                  name = ${Expr(label)},
                  jsonReader = searchJsonReader[discriminator]
                )
                .selectReader[T] { discriminator =>
                  ${
                    Match(
                      '{discriminator}.asTerm,
                      children.zip(discriminatorRefs).map((tpe, branchDiscriminator) => tpe.asType match
                        case '[t] =>
                          CaseDef(
                            branchDiscriminator,
                            None,
                            Typed('{searchJsonReader[t]}.asTerm, TypeTree.of[JsonReader[? <: T]])
                          )
                    ) :+ CaseDef(Wildcard(), None,
                        '{ReaderError.wrongJson(s"Unexpected discriminator found: $discriminator")(using FieldName(${Expr(label)})) }.asTerm
                      )
                    ).asExprOf[JsonReader[? <: T]]
                  }
                }
            }.asTerm
          ).asExprOf[JsonReader[T]]

      case None =>
        report.errorAndAbort("Discriminator is required to derive JsonReader for sum type. Use JsonConfig[T].discriminateBy(_.field)")

  private def distinct(tpes: List[TypeRepr]) =
    tpes.foldLeft(List.empty[TypeRepr]) { (acc, tpe) =>
      if (acc.exists(_ =:= tpe)) acc
      else tpe :: acc
    }

  private def deriveMissingReaders(tpes: List[TypeRepr]): List[ValDef] =
    distinct(tpes).zipWithIndex
      .flatMap { (tpe, idx) =>
        tpe.asType match
          case '[t] =>
            Some(
              ValDef(
                Symbol.newVal(
                  Symbol.spliceOwner,
                  s"given_jsonReader_$idx",
                  TypeRepr.of[JsonReader[t]],
                  Flags.Given | Flags.Lazy | Flags.Implicit,
                  Symbol.noSymbol
                ),
                Some(lookupOpt[JsonReader[t]].getOrElse('{ JsonReader.derived[t](using ${ lookup[scala.deriving.Mirror.Of[t]] }) }).asTerm)
              )
            )
    }
