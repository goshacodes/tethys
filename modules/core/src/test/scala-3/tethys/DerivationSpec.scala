package tethys

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.commons.TokenNode.obj
import tethys.commons.{Token, TokenNode}
import tethys.readers.tokens.QueueIterator
import tethys.writers.tokens.SimpleTokenWriter.SimpleTokenWriterOps

class DerivationSpec extends AnyFlatSpec with Matchers {
  def read[A: JsonReader](nodes: List[TokenNode]): A = {
    val it = QueueIterator(nodes)
    val res = it.readJson[A].fold(throw _, identity)
    it.currentToken() shouldBe Token.Empty
    res
  }

  it should "compile and correctly write and read product" in {
    case class Person(id: Int, name: String, phone: Option[String], default: String = "") derives JsonObjectWriter, JsonReader

    case class Wrapper(person: Person) derives JsonObjectWriter, JsonReader

    Person(2, "Peter", None).asTokenList shouldBe obj(
      "id" -> 2,
      "name" -> "Peter",
      "default" -> ""
    )

    Wrapper(Person(3, "Parker", None, "abc")).asTokenList shouldBe obj(
      "person" -> obj("id" -> 3, "name" -> "Parker", "default" -> "abc")
    )

    read[Person](obj("id" -> 1, "name" -> "abc")) shouldBe Person(1, "abc", None)

    read[Person](
      obj(
        "abd" -> 3,
        "name" -> "abc",
        "id" -> 1,
        "default" -> "abc"
      )
    ) shouldBe Person(1, "abc", None, "abc")

    read[Wrapper](
      obj(
        "abc" -> 5,
        "person" -> obj("id" -> 3, "name" -> "Parker", "phone" -> "123")
      )
    ) shouldBe Wrapper(Person(3, "Parker", Some("123")))
  }

  it should "compile and correctly write sum" in {
    sealed trait A derives JsonObjectWriter

    case class B(b: Int, i: String) extends A derives JsonObjectWriter

    case class C(c: String) extends A derives JsonObjectWriter


    (B(2, "abc"): A).asTokenList shouldBe obj(
      "b" -> 2,
      "i" -> "abc"
    )
  }

  it should "compile and correctly read/write enum with StringEnumWriter" in {
    enum A derives StringEnumJsonWriter, StringEnumJsonReader:
      case B, C

    A.B.asTokenList shouldBe TokenNode.value("B")
    A.C.asTokenList shouldBe TokenNode.value("C")

    read[A](
      TokenNode.value("B")
    ) shouldBe A.B

    read[A](
      TokenNode.value("C")
    ) shouldBe A.C
  }

  it should "compile and correctly read/write enum with OrdinalEnumWriter" in {
    enum A derives OrdinalEnumJsonWriter, OrdinalEnumJsonReader:
      case B, C

    A.B.asTokenList shouldBe TokenNode.value(0)
    A.C.asTokenList shouldBe TokenNode.value(1)

    read[A](
      TokenNode.value(0)
    ) shouldBe A.B

    read[A](
      TokenNode.value(1)
    ) shouldBe A.C
  }

  it should "compile and correcly write enum obj with discriminator" in {
    enum A:
      case B, C

    {
      given JsonWriter[A] = StringEnumJsonWriter.withLabel("__type")
      A.B.asTokenList shouldBe obj("__type" -> "B")
      A.C.asTokenList shouldBe obj("__type" -> "C")
    }

    {
      given JsonWriter[A] = OrdinalEnumJsonWriter.withLabel("__type")

      A.B.asTokenList shouldBe obj("__type" -> 0)
      A.C.asTokenList shouldBe obj("__type" -> 1)
    }
  }

  it should "correctly read case classes with default parameters" in {
    object Mod {
      case class WithOpt(x: Int, y: Option[String] = Some("default")) derives JsonReader
    }

    read[Mod.WithOpt](obj("x" -> 5)) shouldBe Mod.WithOpt(5)
  }

  it should "correctly read case classes with default parameters and type arguments" in {
    case class WithArg[A](x: Int, y: Option[A] = None) derives JsonReader

    read[WithArg[Int]](obj("x" -> 5)) shouldBe WithArg[Int](5)
    read[WithArg[String]](obj("x" -> 5, "y" -> "lool")) shouldBe WithArg[String](5, Some("lool"))
  }

  it should "write/read sum types with provided json discriminator" in {
    enum Disc derives StringEnumJsonWriter, StringEnumJsonReader:
      case A, B

    sealed trait Choose(val discriminator: Disc) derives JsonObjectWriter, JsonReader

    object Choose:
      given JsonDiscriminator[Choose, Disc] = JsonDiscriminator.by(_.discriminator)

      case class AA() extends Choose(Disc.A)
      case class BB() extends Choose(Disc.B)
      
    (Choose.AA(): Choose).asTokenList shouldBe obj("discriminator" -> "A")
    (Choose.BB(): Choose).asTokenList shouldBe obj("discriminator" -> "B")

    read[Choose](obj("discriminator" -> "A")) shouldBe Choose.AA()
    read[Choose](obj("discriminator" -> "B")) shouldBe Choose.BB()
  }

  it should "write/read sum types with provided json discriminator of simple type" in {
    sealed trait Choose(val discriminator: Int) derives JsonObjectWriter, JsonReader

    object Choose:
      given JsonDiscriminator[Choose, Int] = JsonDiscriminator.by(_.discriminator)

      case class AA() extends Choose(0)

      case class BB() extends Choose(1)

    (Choose.AA(): Choose).asTokenList shouldBe obj("discriminator" -> 0)
    (Choose.BB(): Choose).asTokenList shouldBe obj("discriminator" -> 1)

    read[Choose](obj("discriminator" -> 0)) shouldBe Choose.AA()
    read[Choose](obj("discriminator" -> 1)) shouldBe Choose.BB()
  }

  it should "write/read json for generic discriminators" in {
    enum Disc1 derives StringEnumJsonWriter, StringEnumJsonReader:
      case A, B

    enum Disc2 derives StringEnumJsonWriter, StringEnumJsonReader:
      case AA, BB

    sealed trait Choose[A](val discriminator: A) derives JsonWriter, JsonReader

    object Choose:
      given [A]: JsonDiscriminator[Choose[A], A] = JsonDiscriminator.by(_.discriminator)

    case class ChooseA() extends Choose[Disc1](Disc1.A)
    case class ChooseB() extends Choose[Disc2](Disc2.BB)

    (ChooseA(): Choose[Disc1]).asTokenList shouldBe obj("discriminator" -> "A")
    (ChooseB(): Choose[Disc2]).asTokenList shouldBe obj("discriminator" -> "BB")

    read[Choose[Disc1]](obj("discriminator" -> "A")) shouldBe ChooseA()
    read[Choose[Disc2]](obj("discriminator" -> "BB")) shouldBe ChooseB()
  }

  it should "not compile derivation when discriminator override found" in {

    """
      |
      |    sealed trait Foo(val x: Int) derives JsonReader, JsonObjectWriter
      |
      |    object Foo:
      |      given JsonDiscriminator[Foo, Int] = JsonDiscriminator.by(_.x)
      |
      |      case class Bar(override val x: Int) extends Foo(x)
      |
      |      case class Baz() extends Foo(0)
      |
      |""" shouldNot compile


  }
}