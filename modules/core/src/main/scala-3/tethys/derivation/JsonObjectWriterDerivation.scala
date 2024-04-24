package tethys.derivation

import tethys.{JsonDiscriminator, JsonObjectWriter, JsonWriter}
import tethys.writers.tokens.TokenWriter

import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonFrom, summonInline, constValueTuple}

private[tethys] trait JsonObjectWriterDerivation:
  inline def derived[A](using mirror: Mirror.Of[A]): JsonObjectWriter[A] =
    new JsonObjectWriter[A]:
      override def writeValues(value: A, tokenWriter: TokenWriter): Unit =
        inline mirror match
          case m: Mirror.ProductOf[A] =>
            val product = summonInline[A <:< Product](value)
            product.productElementNames
              .zip(product.productIterator)
              .zip(summonJsonWritersForProduct[A, m.MirroredElemTypes])
              .foreach { case ((name, value), writer) =>
                writer.write(name, value.asInstanceOf, tokenWriter)
              }

          case m: Mirror.SumOf[A] =>
            writeDiscriminatorIfProvided[A, m.MirroredElemTypes, m.MirroredElemLabels](value, tokenWriter)

            summonJsonWritersForSum[A, m.MirroredElemTypes](m.ordinal(value))
              .writeValues(value.asInstanceOf, tokenWriter)

  private inline def writeDiscriminatorIfProvided[T, Elems <: Tuple, Labels <: Tuple](value: T, tokenWriter: TokenWriter): Unit =
    summonFrom[JsonDiscriminator[T, _]] {
      case discriminator: JsonDiscriminator[T, t] =>
        summonInline[JsonWriter[t]]
          .write(
            name = Discriminator.getLabel[T, t],
            value = discriminator.choose(value).asInstanceOf[t],
            tokenWriter = tokenWriter
          )

      case _ =>
    }

  private inline def summonJsonWritersForSum[T, Elems <: Tuple]: List[JsonObjectWriter[?]] =
    inline erasedValue[Elems] match
      case _: EmptyTuple =>
        Nil
      case _: (elem *: elems) =>
        summonOrDeriveJsonWriterForSum[T, elem] :: summonJsonWritersForSum[T, elems]

  private inline def summonJsonWritersForProduct[T, Elems <: Tuple]: List[JsonWriter[?]] =
    inline erasedValue[Elems] match
      case _: EmptyTuple =>
        Nil
      case _: (elem *: elems) =>
        summonOrDeriveJsonWriterForProduct[T, elem] :: summonJsonWritersForProduct[T, elems]

  private inline def summonOrDeriveJsonWriterForSum[T, Elem]: JsonObjectWriter[Elem] =
    summonFrom[JsonWriter[Elem]] {
      case writer: JsonObjectWriter[Elem] =>
        writer
      case writer: JsonWriter[Elem] =>
        scala.compiletime.error("JsonObjectWriter required for the children types, but JsonWriter found")
      case _ =>
        deriveRec[T, Elem]
    }

  private inline def summonOrDeriveJsonWriterForProduct[T, Elem]: JsonWriter[Elem] =
    summonFrom[JsonWriter[Elem]] {
      case writer: JsonWriter[Elem] =>
        writer
      case _ =>
        deriveRec[T, Elem]
    }

  private inline def deriveRec[T, Elem]: JsonObjectWriter[Elem] =
    inline erasedValue[T] match
      case _: Elem =>
        scala.compiletime.error("Recursive derivation is not possible")
      case value =>
        JsonWriter.derived[Elem](using summonInline[Mirror.Of[Elem]])
