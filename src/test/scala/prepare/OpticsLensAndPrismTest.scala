package prepare

import monocle.Prism
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


sealed trait Shape

case class Circle(radius: Double) extends Shape

// need move for compilation prism
case object Square extends Shape

class OpticsLensAndPrismTest extends AnyFlatSpec with Matchers {

  case class Address(street: String, city: String)

  case class Person(name: String, age: Int, address: Address)

  import monocle.Lens
  import monocle.macros.GenLens

  // Линзы для Person и Address
  val nameLens: Lens[Person, String] = GenLens[Person](_.name)
  val ageLens: Lens[Person, Int] = GenLens[Person](_.age)
  val addressLens: Lens[Person, Address] = GenLens[Person](_.address)

  val streetLens: Lens[Address, String] = GenLens[Address](_.street)
  val cityLens: Lens[Address, String] = GenLens[Address](_.city)


  "A Person lens" should "update the name correctly" in {
    val originalPerson = Person("Alice", 28, Address("1 Scala Lane", "Function City"))
    nameLens.get(originalPerson) should be("Alice")
    val updatedPerson = nameLens.replace("Bob")(originalPerson)
    updatedPerson.name should be("Bob")
  }

  it should "update the city correctly" in {
    val originalPerson = Person("Alice", 28, Address("1 Scala Lane", "Function City"))
    (addressLens andThen cityLens).get(originalPerson) should be("Function City")
    val updatedPerson = (addressLens andThen cityLens).replace("Type Town")(originalPerson)
    updatedPerson.address.city should be("Type Town")
  }

  "An Address lens" should "update the street correctly" in {
    val originalAddress = Address("1 Scala Lane", "Function City")
    streetLens.getOption(originalAddress) should be(Some("1 Scala Lane"))
    //    streetLens.getAll(originalAddress) should be(List("1 Scala Lane"))
    val updatedAddress = streetLens.replace("2 Dotty Drive")(originalAddress)
    updatedAddress.street should be("2 Dotty Drive")
  }


  import monocle.macros.GenPrism

  val squarePrism: Prism[Shape, Square.type] = GenPrism[Shape, Square.type]
  val circlePrism: Prism[Shape, Circle] = GenPrism[Shape, Circle]


  "A Square Prism" should "extract Square from Shape" in {
    val shape: Shape = Square
    squarePrism.getOption(shape) should be(Some(Square))
    circlePrism.getOption(shape) should be(None)
  }

  it should "return None when extracting Square from non-Square Shape" in {
    val shape: Shape = Circle(5.0)

    squarePrism.getOption(shape) should be(None)
    squarePrism.replace(Square)(shape) should be(shape)
    circlePrism.getOption(shape) should be(Some(shape))
    circlePrism.replace(Circle(7.0))(shape) should be((Circle(7.0)))

  }

  it should "convert a Square to a Shape" in {
    val square: Square.type = Square
    val shape: Shape = squarePrism.reverseGet(square)
    shape should be(Square)
  }

}