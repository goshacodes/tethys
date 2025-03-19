package tethys.jackson

import tethys._
import tethys.writers.tokens.{TokenWriterSpec, TokenWriterProducer}

class JacksonTokenWriterSpec extends TokenWriterSpec {
  override def producer: TokenWriterProducer = jacksonTokenWriterProducer
}
