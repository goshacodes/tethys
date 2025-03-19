package tethys.jackson

import tethys.writers.tokens.{TokenWriterProducer, JsonWriterSpec}

class JacksonJsonWriterSpec extends JsonWriterSpec {
  override def producer: TokenWriterProducer = jacksonTokenWriterProducer
}
