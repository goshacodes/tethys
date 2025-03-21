package tethys.circe

import tethys.commons.TokenNode
import tethys.readers.tokens.TokenIteratorProducer
import tethys.writers.tokens.SimpleTokenWriter

object SimpleTokenWriterRaw {
  implicit final class SimpleTokenWriterRawOps[A](val a: A) extends AnyVal {
    import tethys._

    def asTokenList(implicit
        jsonWriter: JsonWriter[A],
        iteratorProducer: TokenIteratorProducer
    ): List[TokenNode] = {
      val tokenWriter = new SimpleTokenWriter().withRawJsonSupport
      a.writeJson(tokenWriter)
      tokenWriter.tokens.toList
    }
  }
}
