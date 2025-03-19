package tethys.writers.tokens

trait TokenWriterProducer {
  def produce(): TokenWriter
}

object TokenWriterProducer {
  implicit val given_TokenWriterProducer: TokenWriterProducer =
    new TokenWriterProducer {
      private val writerPool: ThreadLocal[DefaultTokenWriter] =
        new ThreadLocal[DefaultTokenWriter] {
          override def initialValue(): DefaultTokenWriter =
            new DefaultTokenWriter(config = TokenWriterConfig.Default)
        }

      override def produce(): TokenWriter = writerPool.get()
    }
}
