package functor

import data.{Box, Printable}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._

class ContravariantFunctorSpec extends AnyFlatSpec with should.Matchers with ContravariantFunctorLaws {

  import data.Op.Op

  it should "laws" in {
    val op: Op[Int, String] = (x: String) => x.toInt
    identityLawLeft(op).apply("10") shouldBe identityLawRight(op).apply("10")
    identityLawLeft(op).apply("10") shouldBe 10
    compositionLawLeft(op)((x: Double) => x.toInt.toString)((x: Long) => (x + 1).toDouble).apply(10) shouldBe compositionLawRight(op)((x: Double) => x.toInt.toString)((x: Long) => (x + 1).toDouble).apply(10)
    compositionLawLeft(op)((x: Double) => x.toInt.toString)((x: Long) => (x + 1).toDouble).apply(10) shouldBe 11

    val ordering: Ordering[String] = Ordering.String
    List("10", "1", "3", "20").sorted(identityLawLeft(ordering)) shouldBe List("10", "1", "3", "20").sorted(identityLawRight(ordering))
    List("10", "1", "3", "20").sorted(identityLawLeft(ordering)) shouldBe List("1", "10", "20", "3")
    List("10", "1", "3", "20").sorted(compositionLawLeft(ordering)((x: Int) => x.toString)((x: String) => x.toInt)) shouldBe List("10", "1", "3", "20").sorted(compositionLawRight(ordering)((x: Int) => x.toString)((x: String) => x.toInt))

  }

  it should "contravariant" in {

    val doubleToInt: Op[Int, Double] = ContravariantFunctor[Op[Int, *]].contramap((x: String) => x.toInt)((x: Double) => x.toInt.toString)

    doubleToInt(10.0) shouldBe 10
    doubleToInt(10.9) shouldBe 10

    val orderingInt: Ordering[Int] = ContravariantFunctor[Ordering].contramap(Ordering.String)((x: Int) => x.toString)
    List(10, 1, 3, 20).sorted(orderingInt) shouldBe List(1, 10, 20, 3)

    Printable.format(Box(10)) shouldBe "10"
    Printable.format(Box("hello world")) shouldBe "hello world"

    def printableBox2[T](implicit printable: Printable[T]): Printable[Box[T]] = ContravariantFunctor[Printable].contramap(printable)((x: Box[T]) => x.value)

    printableBox2[Int].format(Box(10)) shouldBe "10"
    printableBox2[String].format(Box("hello world")) shouldBe "hello world"
  }

}
