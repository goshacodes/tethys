package tethys


trait JsonDiscriminator[A, B]:
  def choose: A => B

object JsonDiscriminator:
  def by[A, B](f: A => B): JsonDiscriminator[A, B] = new JsonDiscriminator[A, B]:
    override def choose: A => B = f