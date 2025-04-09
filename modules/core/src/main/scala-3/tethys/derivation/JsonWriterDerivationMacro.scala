package tethys.derivation

import scala.quoted.*
import scala.deriving.Mirror
import tethys.{JsonWriter, JsonObjectWriter, WriterBuilder, JsonConfiguration}
import tethys.writers.instances.AllJsonWriters
import tethys.writers.tokens.TokenWriter
import scala.collection.mutable
import tethys.derivation.builder.WriterDerivationConfig

private[derivation] trait JsonWriterDerivationMacro extends ConfigurationMacroUtils:
  import quotes.reflect.*

  def deriveJsonWriterForProduct[T: Type](
      builder: Expr[WriterBuilder[T]]
  ): Expr[JsonObjectWriter[T]] =
    val writer = '{
      new JsonObjectWriter[T]:
        override def writeValues(value: T, tokenWriter: TokenWriter): Unit =
           ${
            tokenizeAndRender[T](
              JsonWriterQueueItem.Value(
                value = 'value.asTerm, 
                builder = Some(builder.asTerm), 
                writeObjectStartEnd = false, 
                recursive = false,
                root = true
              ),
              'tokenWriter
            )
          }
    }.asTerm
    //println(writer.show)
    writer.asExprOf[JsonObjectWriter[T]]

  
  def deriveJsonWriterForSum[T: Type](
      legacyConfig: Option[DiscriminatorConfig]
  ): Expr[JsonObjectWriter[T]] =
    val writer = '{
      new JsonObjectWriter[T]:
        override def writeValues(value: T, tokenWriter: TokenWriter): Unit =
          ${
            tokenizeAndRender[T](
              JsonWriterQueueItem.Value(
                value = 'value.asTerm, 
                builder = None, 
                writeObjectStartEnd = false, 
                recursive = false,
                legacySumConfig = legacyConfig,
                root = true
              ), 
              'tokenWriter
            )
          }
    }
    //println(writer.asTerm.show)
    writer

  def tokenizeAndRender[T: Type](root: JsonWriterQueueItem, out: Expr[TokenWriter]): Expr[Unit] =
    renderTokens(tokenize[T](root), out)
  
  def tokenize[T: Type](root: JsonWriterQueueItem): List[JsonWriterToken] = {
    val config = Expr.summon[JsonConfiguration].getOrElse('{ JsonConfiguration.default })
    // To preserve order we add tokens to work. This is needed because some tokens can be split into more tokens
    // When we are done - we add tokens to processed
    val work = mutable.Stack(root)
    val processed = mutable.ListBuffer.empty[JsonWriterToken]
    
    while work.nonEmpty do
      work.removeLast() match
        case JsonWriterQueueItem.FieldName(name) =>
          processed.prepend(JsonWriterToken.FieldName(name))

        case JsonWriterQueueItem.ObjectStart =>
          processed.prepend(JsonWriterToken.ObjectStart)

        case JsonWriterQueueItem.ObjectEnd =>
          processed.prepend(JsonWriterToken.ObjectEnd)

        case JsonWriterQueueItem.ArrayStart =>
          processed.prepend(JsonWriterToken.ArrayStart)

        case JsonWriterQueueItem.ArrayEnd =>
          processed.prepend(JsonWriterToken.ArrayEnd)

        case JsonWriterQueueItem.Optional(value, name) =>
          processed.prepend(JsonWriterToken.OptionalValue(value, name))

        case JsonWriterQueueItem.Value(value, builderOpt, writeObjectStartEnd, recursive, legacySumConfig, root) =>
          val tpe = value.tpe.widenFieldType
          tpe.asType match { 
            case '[Array[c]] =>
              processed.prepend(JsonWriterToken.Array(value))

            case '[Iterable[c]] =>
              processed.prepend(JsonWriterToken.Iterable(value))

            case '[t] =>
              Expr.summon[JsonWriter[t]] match
                // if there is JsonWriter and it is not root type - we are using it
                case Some(_) if !root =>
                  processed.prepend(JsonWriterToken.tokenOf(value))

                case _ =>
                  Expr.summon[Mirror.Of[t]] match
                    // if this is product - we split it into more tokens
                    case Some('{($mirror): Mirror.ProductOf[t]}) if !recursive =>
                      val builder = builderOpt.map(_.asExprOf[WriterBuilder[t]])
                        .orElse(Expr.summon[WriterBuilder[t]])
                        .getOrElse('{WriterBuilder[t](using $mirror)})

                      val fields = prepareWriterProductFields[t](builder, config)

                      if writeObjectStartEnd then work.append(JsonWriterQueueItem.ObjectStart)

                      fields.foreach { field =>
                        if field.tpe.isOption then
                          work.append(JsonWriterQueueItem.Optional(field.value(value), field.label))
                        else
                          work.append(JsonWriterQueueItem.FieldName(field.label))
                          work.append(
                            JsonWriterQueueItem.Value(
                              value = field.value(value), 
                              builder = None, 
                              writeObjectStartEnd = true, 
                              recursive = isRecursiveTpe(field.tpe.widenFieldType), 
                              legacySumConfig = None
                            )
                          )
                      }

                      if writeObjectStartEnd then work.append(JsonWriterQueueItem.ObjectEnd)
                      
                    // if this is sum - we generate pattern match
                    case Some('{($mirror): Mirror.SumOf[t]}) =>
                      val discriminatorConfig = parseSumConfig[t].discriminator
                      if (legacySumConfig.nonEmpty && discriminatorConfig.nonEmpty)
                        report.errorAndAbort("Only one discriminator is allowed. Please remove legacy WriterDescription config")

                      processed.prepend(JsonWriterToken.PatternMatch(value, getAllChildren(tpe), legacySumConfig.orElse(discriminatorConfig)))

                    case _ =>
                      // if this is union type - we generate pattern match for it, else just plain token
                      extractUnionTypes(tpe) match
                        case tpe :: Nil =>
                          processed.prepend(JsonWriterToken.tokenOf(value))
                        case types =>
                          processed.prepend(JsonWriterToken.PatternMatch(value, types, None))
          }
    val (missingWritersTpes, tokens) = processed.partitionMap {
      case JsonWriterToken.Value(field, None, false) => Left(field.tpe.widenTermRefByName)
      case other => Right(other)
    }
    if missingWritersTpes.nonEmpty then
      report.errorAndAbort(s"Missing writers for: ${missingWritersTpes.map(_.show(using Printer.TypeReprShortCode)).mkString(", ")}")
    else
      tokens.toList
  }

  def renderTokens(tokens: List[JsonWriterToken], out: Expr[TokenWriter]): Expr[Unit] =
    tokens.foldLeft('{})((acc, token) => '{$acc; ${renderToken(token, out)}})

  def renderToken(token: JsonWriterToken, out: Expr[TokenWriter]): Expr[Unit] = 
    token match
      case JsonWriterToken.ObjectStart =>
        '{ $out.writeObjectStart() }

      case JsonWriterToken.ObjectEnd =>
        '{ $out.writeObjectEnd() }

      case JsonWriterToken.ArrayStart =>
        '{ $out.writeArrayStart() }

      case JsonWriterToken.ArrayEnd =>
        '{ $out.writeArrayEnd() }

      case JsonWriterToken.FieldName(name) =>
        '{ $out.writeFieldName($name) }

      case JsonWriterToken.OptionalValue(value, name) =>
        value.tpe.widenFieldType.asType match
          case '[Option[t]] =>
            val v = value.asExprOf[Option[t]]
            def default =
              '{ 
                  if $v.nonEmpty then
                    $out.writeFieldName($name) 
                    ${renderToken(JsonWriterToken.tokenOf('{$v.get}.asTerm), out)}
               }
            Expr.summon[JsonWriter[Option[t]]] match
              case Some('{ (${_}: AllJsonWriters).optionalWriter[tt](${_})}) =>
                 default
              case None =>
                 default
              case Some(writer) =>
                '{ ${ writer.asExprOf[JsonWriter[Option[t]]] }.write($name, $v, $out) }

      case JsonWriterToken.Iterable(value) =>
        val tpe = value.tpe.widenFieldType
        def default[T: Type]: Expr[Unit] =
          '{
              $out.writeArrayStart()
              val it = ${value.asExprOf[Iterable[T]]}.iterator
              while (it.hasNext)
                ${
                  tokenizeAndRender[T](
                    JsonWriterQueueItem.Value(
                      value = '{it.next()}.asTerm, 
                      builder = None, 
                      writeObjectStartEnd = true, 
                      recursive = false,
                      legacySumConfig = None
                    ), 
                    out
                  )
                }
              $out.writeArrayEnd()
              ()
            }
        val search = tpe match
          case AppliedType(tycon, types) =>
             AppliedType(tycon, List(TypeRepr.of[Int])).asType match
              case '[exactType] =>
                println(Expr.summon[JsonWriter[exactType]].map(_.asTerm.show(using Printer.TreeStructure)))
          case other => other
        
        (tpe.asType, tpe.asType) match
          case ('[exactType], '[Iterable[t]]) =>
            println(tpe.show(using Printer.TypeReprStructure))
            println(Expr.summon[JsonWriter[?] ?=> JsonWriter[exactType]].map(_.asTerm.show))
            Expr.summon[JsonWriter[exactType]] match
              case Some('{ (${_}: AllJsonWriters).iterableWriter(${_})}) =>
                 default[t]
              case None =>
                 default[t]
              case Some(writer) =>
                '{ ${ writer.asExprOf[JsonWriter[exactType]] }.write(${value.asExprOf[exactType]}, $out) }
            
            
      case JsonWriterToken.Array(value) =>
        '{}

      case JsonWriterToken.Value(value, writerOpt, writerOmitted) =>
        val simplifiedTpe = value.tpe.widenFieldType
        simplifiedTpe.asType match
          case '[t] =>
            writerOpt match
              case Some(writer) =>
                '{ ${ writer.asExprOf[JsonWriter[t]] }.write(${ value.asExprOf[t] }, $out) }

              case None if value.tpe <:< TypeRepr.of[String] =>
                '{ $out.writeString(${value.asExprOf[String]})}

              case None if value.tpe <:< TypeRepr.of[Int] =>
                '{ $out.writeNumber(${value.asExprOf[Int]})}

              case None if value.tpe <:< TypeRepr.of[Long] =>
                '{ $out.writeNumber(${value.asExprOf[Long]})}

              case None if value.tpe <:< TypeRepr.of[Float] =>
                '{ $out.writeNumber(${value.asExprOf[Float]})}

              case None if value.tpe <:< TypeRepr.of[Double] =>
                '{ $out.writeNumber(${value.asExprOf[Double]})}

              case None if value.tpe <:< TypeRepr.of[Boolean] =>
                '{ $out.writeBoolean(${value.asExprOf[Boolean]})}

              case None =>
                  report.errorAndAbort(s"Unhandled ommited JsonWriter for ${simplifiedTpe.show}")

      case JsonWriterToken.PatternMatch(value, types, discriminatorConfig) =>
        value.underlying match
          // reduce nested pattern matches for WriterBuilder
          case Match(selector, cases) =>
            Match(
              selector = selector, 
              cases = cases.map { caseDef =>
                CaseDef(
                  caseDef.pattern,
                  caseDef.guard,
                  renderToken(JsonWriterToken.tokenOf(caseDef.rhs), out).asTerm
                ) 
              }
            ).asExprOf[Unit]
          // write discriminator and then pattern match by type
          case other =>
            val discriminatorTokens = discriminatorConfig match {
              case None => Nil
              case Some(DiscriminatorConfig(label, discriminatorTpe, values)) =>
                List(
                  JsonWriterToken.FieldName(Expr(label)),
                  values match
                    // if it is legacy discriminator from WriterDescription - we generate pattern match for it
                    case None =>
                      JsonWriterToken.tokenOf(
                        Match(
                          selector = value, 
                          cases = types.map { tpe =>
                            tpe.asType match
                              case '[oneOf] =>
                                val bindSymbol = Symbol.newBind(Symbol.spliceOwner, "v", Flags.EmptyFlags, tpe)
                                CaseDef(
                                  pattern = Bind(bindSymbol, Typed(Wildcard(), TypeTree.of[oneOf])),
                                  guard = None,
                                  rhs = Literal(StringConstant(tpe.typeSymbol.name.filterNot(_ == '$')))
                                )
                          }
                        )
                      )
                    // if it is @selector annotation - we just write discriminator value
                    case Some(values) =>
                       JsonWriterToken.tokenOf(Select.unique(value, label))
                )
            }
              
            val writeByTypeMatch = Match(
              selector = value,
              cases = types.map { tpe =>
              tpe.asType match
                case '[oneOf] =>
                  val bindSymbol = Symbol.newBind(Symbol.spliceOwner, "v", Flags.EmptyFlags, tpe) 
                  val subTypeValue = Ref(bindSymbol).asExprOf[oneOf]
                  CaseDef(
                    pattern = Bind(bindSymbol, Typed(Wildcard(), TypeTree.of[oneOf])),
                    guard = None,
                    rhs =
                        Expr.summon[JsonObjectWriter[oneOf]] match
                          case Some(writer) =>
                            '{ $writer.writeValues($subTypeValue, $out) }.asTerm
                          
                          case None =>
                            tokenizeAndRender[oneOf](
                              JsonWriterQueueItem.Value(
                                value = subTypeValue.asTerm, 
                                builder = None, 
                                writeObjectStartEnd = false, 
                                recursive = false
                              ),
                              out
                            ).asTerm
                  )
              }
            )
            '{ 
             ${renderTokens(discriminatorTokens, out)}
             ${writeByTypeMatch.asExprOf[Unit]} 
            }

  enum JsonWriterToken:
    case ObjectStart, ObjectEnd, ArrayStart, ArrayEnd
    case FieldName(name: Expr[String])
    case Value(field: Term, writer: Option[Term], writerOmitted: Boolean)
    case OptionalValue(field: Term, name: Expr[String])
    case PatternMatch(value: Term, types: List[TypeRepr], discriminatorConfig: Option[DiscriminatorConfig])
    case Iterable(field: Term)
    case Array(field: Term)

  object JsonWriterToken:
    def tokenOf(value: Term): JsonWriterToken.Value =
      val tpe = value.tpe.widenFieldType
      tpe.asType match { case '[t] =>
        val writer = Expr.summon[JsonWriter[t]]

        val known = writer.exists {
          case '{(${_}: AllJsonWriters).stringWriter} => true
          case '{(${_}: AllJsonWriters).intWriter} => true
          case '{(${_}: AllJsonWriters).longWriter} => true
          case '{(${_}: AllJsonWriters).floatWriter} => true
          case '{(${_}: AllJsonWriters).doubleWriter} => true
          case '{(${_}: AllJsonWriters).booleanWriter} => true
          case _ => false
        }
        JsonWriterToken.Value(value, writer.filterNot(_ => known).map(_.asTerm), known)
      }

  enum JsonWriterQueueItem:
    case Value(
      value: Term, 
      builder: Option[Term],
      writeObjectStartEnd: Boolean, 
      recursive: Boolean, 
      legacySumConfig: Option[DiscriminatorConfig] = None,
      root: Boolean = false
    )
    case Optional(value: Term, name: Expr[String])
    case FieldName(name: Expr[String])
    case ObjectStart, ObjectEnd, ArrayStart, ArrayEnd

  extension (tpe: TypeRepr)
    def widenFieldType: TypeRepr =
      tpe.widenTermRefByName.dealias
    

