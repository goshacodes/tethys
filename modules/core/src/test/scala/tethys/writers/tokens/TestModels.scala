package tethys.writers.tokens

object TestModels {
  case class Person(name: String, age: Int, email: Option[String])

  case class Address(
      street: String,
      city: String,
      country: String,
      postalCode: Option[String]
  )

  case class Employee(
      id: Long,
      person: Person,
      address: Address,
      department: String,
      salary: BigDecimal
  )

  case class Department(
      name: String,
      employees: List[Employee],
      tags: Set[String],
      metadata: Map[String, String]
  )

  case class Container[T](
      id: String,
      data: T,
      timestamp: Long
  )

  case class TreeNode(
      value: String,
      children: List[TreeNode]
  )

  case class CustomTypes(
      uuid: java.util.UUID,
      date: java.time.LocalDate,
      datetime: java.time.LocalDateTime,
      bigInt: BigInt,
      bigDecimal: BigDecimal,
      duration: java.time.Duration
  )
}
