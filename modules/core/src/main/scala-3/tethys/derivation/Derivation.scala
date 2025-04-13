package tethys.derivation

import tethys.derivation.builder.{ReaderDerivationConfig, WriterDerivationConfig}
import tethys.writers.tokens.TokenWriter
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator
import tethys.{
  JsonObjectWriter,
  JsonReader,
  JsonWriter,
  ReaderBuilder,
  WriterBuilder,
  JsonConfiguration
}
import scala.Tuple2
import scala.annotation.tailrec
import scala.compiletime.{constValueTuple, summonInline}
import scala.quoted.*
import scala.collection.mutable
import scala.deriving.Mirror
import tethys.writers.instances.AllJsonWriters

private[tethys] object Derivation:

  inline def writeProductTokens[T](
      inline config: WriterBuilder[T],
      value: T,
      out: TokenWriter
  ): Unit =
    ${ DerivationMacro.writeProductTokens[T]('config, 'value, 'out) }

  inline def writeSumTokens[T](value: T, out: TokenWriter): Unit =
    ${ DerivationMacro.writeSumTokens[T]('value, 'out) }

  @deprecated
  inline def writeProductTokensLegacy[T](
      inline config: WriterDerivationConfig,
      value: T,
      out: TokenWriter
  )(using mirror: Mirror.ProductOf[T]): Unit =
    ${
      DerivationMacro
        .writeProductTokensLegacy[T]('config, 'mirror, 'value, 'out)
    }

  @deprecated
  inline def writeSumTokensLegacy[T](
      inline config: WriterDerivationConfig,
      value: T,
      out: TokenWriter
  ): Unit =
    ${ DerivationMacro.writeSumTokensLegacy[T]('config, 'value, 'out) }

  inline def deriveJsonReaderForSum[T]: JsonReader[T] =
    ${ DerivationMacro.deriveJsonReaderForSum[T] }

  inline def deriveJsonReaderForProduct[T](
      inline config: ReaderBuilder[T],
      inline jsonConfig: JsonConfiguration
  ): JsonReader[T] =
    ${
      DerivationMacro
        .deriveJsonReaderForProduct[T]('{ config }, '{ jsonConfig })
    }

  @deprecated
  inline def deriveJsonReaderForProductLegacy[T](
      inline config: ReaderDerivationConfig
  )(using mirror: Mirror.ProductOf[T]): JsonReader[T] =
    ${
      DerivationMacro
        .deriveJsonReaderForProductLegacy[T]('{ config }, '{ mirror })
    }

object DerivationMacro:
  def writeProductTokens[T: Type](
      config: Expr[WriterBuilder[T]],
      value: Expr[T],
      out: Expr[TokenWriter]
  )(using
      ctx: Quotes
  ): Expr[Unit] =
    new JsonWriterDerivationMacro { val quotes = ctx }
      .tokenize[T](value, Some(config), out, None)

  def writeSumTokens[T: Type](
      value: Expr[T],
      out: Expr[TokenWriter]
  )(using
      ctx: Quotes
  ): Expr[Unit] =
    new JsonWriterDerivationMacro { val quotes = ctx }
      .tokenize[T](value, None, out, None)

  @deprecated
  def writeProductTokensLegacy[T: Type](
      config: Expr[WriterDerivationConfig],
      mirror: Expr[Mirror.ProductOf[T]],
      value: Expr[T],
      out: Expr[TokenWriter]
  )(using ctx: Quotes): Expr[Unit] =
    val derivation = new JsonWriterDerivationMacro { val quotes = ctx }
    derivation.tokenize[T](
      value,
      Some(derivation.parseLegacyWriterDerivationConfig[T](config, mirror)),
      out,
      None
    )

  @deprecated
  def writeSumTokensLegacy[T: Type](
      config: Expr[WriterDerivationConfig],
      value: Expr[T],
      out: Expr[TokenWriter]
  )(using ctx: Quotes): Expr[Unit] =
    val derivation = new JsonWriterDerivationMacro { val quotes = ctx }
    derivation.tokenize[T](
      value,
      None,
      out,
      Some(derivation.parseLegacyDiscriminator[T](config))
    )

  def deriveJsonReaderForProduct[T: Type](
      config: Expr[ReaderBuilder[T]],
      jsonConfig: Expr[JsonConfiguration]
  )(using
      quotes: Quotes
  ): Expr[JsonReader[T]] =
    new DerivationMacro(quotes)
      .deriveJsonReaderForProduct[T](config, jsonConfig)

  def deriveJsonReaderForSum[T: Type](using
      quotes: Quotes
  ): Expr[JsonReader[T]] =
    new DerivationMacro(quotes).deriveJsonReaderForSum[T]

  @deprecated
  def deriveJsonReaderForProductLegacy[T: Type](
      config: Expr[ReaderDerivationConfig],
      mirror: Expr[Mirror.ProductOf[T]]
  )(using quotes: Quotes): Expr[JsonReader[T]] =
    new DerivationMacro(quotes)
      .deriveJsonReaderForProductLegacy[T](config, mirror)

private[derivation] class DerivationMacro(val quotes: Quotes) extends JsonWriterDerivationMacro:
  import quotes.reflect.*

  def deriveJsonReaderForProduct[T: Type](
      config: Expr[ReaderBuilder[T]],
      jsonConfig: Expr[JsonConfiguration]
  ): Expr[JsonReader[T]] =
    val tpe = TypeRepr.of[T]
    val (fields, isStrict) = prepareReaderProductFields[T](config, jsonConfig)
    val existingLabels = fields.map(_.name).toSet
    val fieldsWithoutReader = fields.collect {
      case field: ReaderField.Extracted if field.reader => field.name
    }

    val (basicFields, extractedFields) = fields.partitionMap {
      case field: ReaderField.Basic     => Left(field)
      case field: ReaderField.Extracted => Right(field)
    }

    val expectedFieldNames =
      basicFields.map(_.name).toSet ++ extractedFields.flatMap(
        _.extractors.map(_._1)
      ) -- extractedFields.map(_.name)

    def failIfNotInitialized(fieldName: Expr[FieldName]): Expr[Unit] =
      basicFields.filterNot(_.default.nonEmpty) match
        case refs @ head :: tail =>
          val boolExpr = tail.foldLeft('{
            !${ head.initRef.asExprOf[Boolean] }
          }) { (acc, el) =>
            '{ ${ acc } || !${ el.initRef.asExprOf[Boolean] } }
          }
          '{
            if { $boolExpr } then
              val uninitializedFields =
                new scala.collection.mutable.ArrayBuffer[String](${
                  Expr(refs.size)
                })
              ${
                Expr.block(
                  refs.map { ref =>
                    '{
                      if !${ ref.initRef.asExprOf[Boolean] } then
                        uninitializedFields += ${ Expr(ref.name) }
                    }
                  },
                  '{}
                )
              }
              ReaderError.wrongJson(
                "Can not extract fields from json: " + uninitializedFields
                  .mkString("'", "', '", "'")
              )(${ fieldName })
          }

        case Nil =>
          '{}

    if tpe.typeSymbol.flags.is(Flags.Module) then
      '{ JsonReader.const(${ Ref(tpe.termSymbol).asExprOf[T] }) }
    else
      val (missingReaders, refs) =
        deriveMissingReaders(tpe, basicFields.map(_.tpe))
      val term = Block(
        missingReaders,
        '{
          new JsonReader[T]:
            given JsonReader[T] = this
            override def read(it: TokenIterator)(using fieldName: FieldName) =
              if !it.currentToken().isObjectStart then
                ReaderError.wrongJson(
                  "Expected object start but found: " + it
                    .currentToken()
                    .toString
                )
              else
                it.nextToken()
                ${
                  Block(
                    fields.flatMap(_.initialize),
                    '{
                      while (!it.currentToken().isObjectEnd)
                        val jsonName = it.fieldName()
                        it.nextToken()
                        ${
                          Match(
                            selector = '{ jsonName }.asTerm,
                            cases = fields.flatMap(
                              _.initializeFieldCase(
                                refs,
                                '{ it },
                                '{ fieldName }
                              )
                            ) :+
                              CaseDef(
                                Wildcard(),
                                None,
                                if isStrict then
                                  '{
                                    ReaderError.wrongJson(
                                      s"unexpected field '$jsonName', expected one of ${${
                                          Expr(expectedFieldNames.mkString("'", "', '", "'"))
                                        }}"
                                    )
                                  }.asTerm
                                else '{ it.skipExpression(); () }.asTerm
                              )
                          ).asExprOf[Unit]
                        }
                      it.nextToken()

                      ${ failIfNotInitialized('{ fieldName }) }

                      ${
                        val allRefs =
                          fields.map(field => field.name -> field.ref).toMap
                        Expr.block(
                          extractedFields
                            .flatMap(_.extract(allRefs, '{ fieldName }))
                            .map(_.asExprOf[Unit]),
                          '{}
                        )
                      }

                      ${
                        New(TypeTree.of[T])
                          .select(tpe.classSymbol.get.primaryConstructor)
                          .appliedToTypes(tpe.typeArgs)
                          .appliedToArgs(
                            fields
                              .filterNot(_.idx == -1)
                              .sortBy(_.idx)
                              .map(_.ref)
                          )
                          .asExprOf[T]
                      }

                    }.asTerm
                  ).asExprOf[T]
                }
        }.asTerm
      )
      term.asExprOf[JsonReader[T]]

  def deriveJsonReaderForSum[T: Type]: Expr[JsonReader[T]] =
    val tpe = TypeRepr.of[T]
    val parsed = parseSumConfig[T]
    val children = getAllChildren(tpe)
    parsed.discriminator match
      case Some(DiscriminatorConfig(label, tpe, Some(discriminators))) =>
        tpe.asType match
          case '[discriminator] =>
            val (discriminatorStats, discriminatorRefs) =
              discriminators.zipWithIndex
                .map((term, idx) =>
                  val stat = ValDef(
                    Symbol.newVal(
                      Symbol.spliceOwner,
                      s"Discriminator_$idx",
                      term.tpe,
                      Flags.Private,
                      Symbol.noSymbol
                    ),
                    Some(term)
                  )
                  (stat, Ref(stat.symbol))
                )
                .unzip
            val (readers, refs) = deriveMissingReaders(TypeRepr.of[T], children)
            val term = Block(
              readers ++ discriminatorStats,
              '{
                JsonReader.builder
                  .addField[discriminator](
                    name = ${ Expr(label) },
                    jsonReader = ${ lookup[JsonReader[discriminator]] }
                  )
                  .selectReader[T] { discriminator =>
                    ${
                      Match(
                        '{ discriminator }.asTerm,
                        children
                          .zip(discriminatorRefs)
                          .map((tpe, branchDiscriminator) =>
                            tpe.asType match
                              case '[t] =>
                                CaseDef(
                                  branchDiscriminator,
                                  None,
                                  Typed(
                                    refs.getOrElse(
                                      tpe,
                                      lookup[JsonReader[t]].asTerm
                                    ),
                                    TypeTree.of[JsonReader[? <: T]]
                                  )
                                )
                          ) :+ CaseDef(
                          Wildcard(),
                          None,
                          '{
                            ReaderError.wrongJson(
                              s"Unexpected discriminator found: $discriminator"
                            )(using FieldName(${ Expr(label) }))
                          }.asTerm
                        )
                      ).asExprOf[JsonReader[? <: T]]
                    }
                  }
              }.asTerm
            )
            term.asExprOf[JsonReader[T]]

      case _ =>
        report.errorAndAbort(
          "Discriminator is required to derive JsonReader for sum type. Use @selector annotation"
        )

  private def distinct(tpes: List[TypeRepr]) =
    tpes.foldLeft(List.empty[TypeRepr]) { (acc, tpe) =>
      if (acc.exists(_ =:= tpe)) acc
      else tpe :: acc
    }

  private def deriveMissingReaders(
      thisTpe: TypeRepr,
      tpes: List[TypeRepr]
  ): (List[ValDef], Map[TypeRepr, Ref]) =
    val (stats, refs) = distinct(tpes)
      .filterNot(isRecursive(thisTpe, _))
      .flatMap { tpe =>
        tpe.asType match
          case '[t] =>
            lookupOpt[JsonReader[t]].map {
              _.asTerm match
                case ident: Ident =>
                  Left(ident)
                case other =>
                  Right(other)
            } match
              case Some(Left(_)) =>
                None

              case other =>
                val valDef = ValDef(
                  Symbol.newVal(
                    Symbol.spliceOwner,
                    s"given_JsonReader_${tpe.show(using Printer.TypeReprShortCode)}",
                    TypeRepr.of[JsonReader[t]],
                    Flags.Given,
                    Symbol.noSymbol
                  ),
                  Some(
                    other
                      .flatMap(_.toOption)
                      .getOrElse {
                        '{
                          JsonReader.derived[t](using
                            ${ lookup[scala.deriving.Mirror.Of[t]] }
                          )
                        }.asTerm
                      }
                  )
                )
                Some((valDef, (tpe, Ref(valDef.symbol))))
      }
      .unzip
    (stats, refs.toMap)

  @deprecated
  def deriveJsonReaderForProductLegacy[T: Type](
      config: Expr[ReaderDerivationConfig],
      mirror: Expr[Mirror.ProductOf[T]]
  ): Expr[JsonReader[T]] =
    deriveJsonReaderForProduct(
      parseLegacyReaderDerivationConfig(config, mirror),
      '{ JsonConfiguration.default }
    )
