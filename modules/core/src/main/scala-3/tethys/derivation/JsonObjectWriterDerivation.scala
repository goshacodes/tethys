package tethys.derivation

import tethys.derivation.builder.WriterDerivationConfig

import scala.deriving.Mirror
import tethys.{JsonObjectWriter, JsonWriter, WriterBuilder, JsonConfiguration}
import tethys.writers.tokens.TokenWriter

import scala.deriving.Mirror
import scala.compiletime.{
  constValueTuple,
  erasedValue,
  summonFrom,
  summonInline
}

private[tethys] trait JsonObjectWriterDerivation:

  inline def derived[A](inline config: WriterBuilder[A])(using
      mirror: Mirror.ProductOf[A]
  ): JsonObjectWriter[A] = new JsonObjectWriter[A]:
    override def writeValues(value: A, tokenWriter: TokenWriter): Unit =
      Derivation.writeProductTokens[A](config, value, tokenWriter)

  @deprecated("Use WriterBuilder instead")
  inline def derived[A](inline config: WriterDerivationConfig)(using
      mirror: Mirror.Of[A]
  ): JsonObjectWriter[A] =
    new JsonObjectWriter[A]:
      override def writeValues(value: A, tokenWriter: TokenWriter): Unit =
        inline mirror match
          case given Mirror.ProductOf[A] =>
            Derivation.writeProductTokensLegacy[A](config, value, tokenWriter)

          case given Mirror.SumOf[A] =>
            Derivation.writeSumTokensLegacy[A](config, value, tokenWriter)
    

  inline def derived[A](using inline mirror: Mirror.Of[A]): JsonObjectWriter[A] =
    new JsonObjectWriter[A]:
      def writeValues(value: A, tokenWriter: TokenWriter): Unit =
        inline mirror match
          case given Mirror.ProductOf[A] =>
            Derivation.writeProductTokens[A](
              summonFrom[WriterBuilder[A]] {
                case config: WriterBuilder[A] =>
                  config
                case _ => WriterBuilder[A]
              },
              value,
              tokenWriter
            )

          case given Mirror.SumOf[A] =>
            Derivation.writeSumTokens[A](value, tokenWriter)
