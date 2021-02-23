package functor

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._

class ContravariantFunctorSpec extends AnyFlatSpec with should.Matchers {

  it should "contravariant" in {
    import data.Op.Op
    import functor.ContravariantFunctor._

    val doubleToInt: Op[Int, Double] = ContravariantFunctor[Op[Int, *]].contramap((x: String) => x.toInt)((x: Double) => x.toInt.toString)

    doubleToInt(10.0) shouldBe 10
    doubleToInt(10.9) shouldBe 10

    val orderingInt: Ordering[Int] = ContravariantFunctor[Ordering].contramap(Ordering.String)((x: Int) => x.toString)
    List(10, 1, 3, 20).sorted(orderingInt) shouldBe List(1, 10, 20, 3)
  }

}
