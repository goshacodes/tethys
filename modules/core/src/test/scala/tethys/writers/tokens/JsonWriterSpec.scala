package tethys.writers.tokens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys._

class DefaultJsonWriterSpec extends JsonWriterSpec {
  override def producer: TokenWriterProducer =
    TokenWriterProducer.given_TokenWriterProducer
}

trait JsonWriterSpec extends AnyFlatSpec with Matchers {

  import TestModels._
  import java.time.{LocalDate, LocalDateTime, Duration}
  import java.util.UUID

  def producer: TokenWriterProducer

  def testCase[A: JsonWriter](
      value: A,
      json: String
  )(implicit pos: org.scalactic.source.Position): Unit =
    it should s"write $value as $json" in {
      value.asJson.filterNot(Set('\n', '\t', ' ')) shouldBe json.filterNot(
        Set('\n', '\t', ' ')
      )
    }

  testCase(
    value = 2,
    json = "2"
  )

  testCase(
    value = 1L,
    json = "1"
  )

  testCase(
    value = 1.0,
    json = "1.0"
  )

  testCase(
    value = "test",
    json = """"test""""
  )

  testCase(
    value = true,
    json = "true"
  )

  testCase(
    value = false,
    json = "false"
  )

  testCase(
    value = null,
    json = "null"
  )

  testCase(
    value = List(1, 2, 3),
    json = "[1,2,3]"
  )

  testCase(
    value = Map("a" -> 1, "b" -> 2),
    json = """{"a":1,"b":2}"""
  )

  testCase(
    value = Option(1),
    json = "1"
  )

  testCase(
    value = Option.empty[Int],
    json = "null"
  )

  implicit val uuidWriter: JsonWriter[UUID] =
    JsonWriter.stringWriter.contramap(_.toString)

  implicit val localDateWriter: JsonWriter[LocalDate] =
    JsonWriter.stringWriter.contramap(_.toString)

  implicit val localDateTimeWriter: JsonWriter[LocalDateTime] =
    JsonWriter.stringWriter.contramap(_.toString)

  implicit val durationWriter: JsonWriter[Duration] =
    JsonWriter.stringWriter.contramap(_.toString)

  implicit val personWriter: JsonWriter[Person] = JsonWriter
    .obj[Person]
    .addField("name")(_.name)
    .addField("age")(_.age)
    .addField("email")(_.email)

  implicit val addressWriter: JsonWriter[Address] = JsonWriter
    .obj[Address]
    .addField("street")(_.street)
    .addField("city")(_.city)
    .addField("country")(_.country)
    .addField("postalCode")(_.postalCode)

  implicit val employeeWriter: JsonWriter[Employee] = JsonWriter
    .obj[Employee]
    .addField("id")(_.id)
    .addField("person")(_.person)
    .addField("address")(_.address)
    .addField("department")(_.department)
    .addField("salary")(_.salary)

  implicit val departmentWriter: JsonWriter[Department] = JsonWriter
    .obj[Department]
    .addField("name")(_.name)
    .addField("employees")(_.employees)
    .addField("tags")(_.tags)
    .addField("metadata")(_.metadata)

  implicit def containerWriter[T: JsonWriter]: JsonWriter[Container[T]] =
    JsonWriter
      .obj[Container[T]]
      .addField("id")(_.id)
      .addField("data")(_.data)
      .addField("timestamp")(_.timestamp)

  implicit val customTypesWriter: JsonWriter[CustomTypes] = JsonWriter
    .obj[CustomTypes]
    .addField("uuid")(_.uuid)
    .addField("date")(_.date)
    .addField("datetime")(_.datetime)
    .addField("bigInt")(_.bigInt)
    .addField("bigDecimal")(_.bigDecimal)
    .addField("duration")(_.duration)

  testCase(
    value = Person("John Doe", 30, Some("john@example.com")),
    json = """
        {
          "name": "John Doe",
          "age": 30,
          "email": "john@example.com"
        }"""
  )

  testCase(
    value = Person("Jane Doe", 25, None),
    json = """
        {
          "name": "Jane Doe",
          "age": 25
        }"""
  )

  val address = Address("123 Main St", "New York", "USA", Some("10001"))
  val person = Person("John Doe", 30, Some("john@example.com"))

  testCase(
    value =
      Employee(1L, person, address, "Engineering", BigDecimal("100000.00")),
    json = """
        {
          "id": 1,
          "person": {
            "name": "John Doe",
            "age": 30,
            "email": "john@example.com"
          },
          "address": {
            "street": "123 Main St",
            "city": "New York",
            "country": "USA",
            "postalCode": "10001"
          },
          "department": "Engineering",
          "salary": 100000.00
        }"""
  )

  testCase(
    value = Department(
      "Engineering",
      List(
        Employee(1L, person, address, "Engineering", BigDecimal("100000.00"))
      ),
      Set("tech", "development"),
      Map("location" -> "Floor 3", "manager" -> "Jane Smith")
    ),
    json = """
        {
          "name": "Engineering",
          "employees": [{
            "id": 1,
            "person": {
              "name": "John Doe",
              "age": 30,
              "email": "john@example.com"
            },
            "address": {
              "street": "123 Main St",
              "city": "New York",
              "country": "USA",
              "postalCode": "10001"
            },
            "department": "Engineering",
            "salary": 100000.00
          }],
          "tags": ["tech", "development"],
          "metadata": {
            "location": "Floor 3",
            "manager": "Jane Smith"
          }
        }"""
  )

  testCase(
    value = Container("123", person, 1234567890L),
    json = """
        {
          "id": "123",
          "data": {
            "name": "John Doe",
            "age": 30,
            "email": "john@example.com"
          },
          "timestamp": 1234567890
        }"""
  )

  testCase(
    value = CustomTypes(
      UUID.fromString("550e8400-e29b-41d4-a716-446655440000"),
      LocalDate.of(2023, 1, 1),
      LocalDateTime.of(2023, 1, 1, 12, 0),
      BigInt("123456789"),
      BigDecimal("123456.789"),
      Duration.ofHours(24)
    ),
    json =
      """{"uuid":"550e8400-e29b-41d4-a716-446655440000","date":"2023-01-01","datetime":"2023-01-01T12:00","bigInt":123456789,"bigDecimal":123456.789,"duration":"PT24H"}"""
  )

  testCase(
    value = Department("Empty", List(), Set(), Map()),
    json = """{"name":"Empty","employees":[],"tags":[],"metadata":{}}"""
  )

  testCase(
    value = Container("empty", List[String](), 0L),
    json = """{"id":"empty","data":[],"timestamp":0}"""
  )

  val deeplyNested = Department(
    "Global Engineering",
    List(
      Employee(
        1L,
        Person("John Doe", 30, Some("john@example.com")),
        Address("123 Main St", "New York", "USA", Some("10001")),
        "Backend",
        BigDecimal("100000.00")
      ),
      Employee(
        2L,
        Person("Jane Smith", 28, None),
        Address("456 Park Ave", "Boston", "USA", None),
        "Frontend",
        BigDecimal("95000.50")
      )
    ),
    Set("global", "engineering", "tech"),
    Map(
      "headquarters" -> "New York",
      "founded" -> "2020",
      "status" -> "active"
    )
  )

}
