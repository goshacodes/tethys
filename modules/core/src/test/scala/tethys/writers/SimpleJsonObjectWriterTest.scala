package tethys.writers

import org.scalatest.matchers.should.Matchers.{value as _, *}
import org.scalatest.flatspec.AnyFlatSpec
import tethys.{JsonObjectWriter, JsonWriter}
import tethys.commons.TokenNode.*
import tethys.writers.SimpleJsonObjectWriterTest.{CharData, TestData}
import tethys.writers.instances.SimpleJsonObjectWriter
import tethys.writers.tokens.SimpleTokenWriter.*

class SimpleJsonObjectWriterTest extends AnyFlatSpec {
  behavior of "SimpleJsonObjectWriter"

  it should "write correct object to TokenWriter" in {
    implicit val testWriter: SimpleJsonObjectWriter[TestData] = {
      JsonWriter
        .obj[TestData]
        .addField("a")(_.a)
        .addField("b")(_.b)
        .addField("c")(_.b.isEmpty)
    }

    TestData(1, "test").asTokenList shouldBe obj(
      "a" -> 1,
      "b" -> "test",
      "c" -> false
    )
  }

  it should "write correct object to TokenWriter for concatenated writers" in {
    implicit val testWriter: JsonObjectWriter[TestData] = {
      JsonWriter
        .obj[TestData]
        .addField("a")(_.a)
        .concat(JsonWriter.obj[TestData].addField("b")(_.b))
        .addField("c")(_.b.isEmpty)
    }

    TestData(1, "test").asTokenList shouldBe obj(
      "a" -> 1,
      "b" -> "test",
      "c" -> false
    )
  }

  it should "write correct object with char field" in {
    implicit val charWriter: SimpleJsonObjectWriter[CharData] = {
      JsonWriter
        .obj[CharData]
        .addField("c")(_.c)
    }

    CharData('c').asTokenList shouldBe obj(
      "c" -> "c"
    )
  }
}

object SimpleJsonObjectWriterTest {

  case class TestData(a: Int, b: String)
  case class CharData(c: Char)

}
