package functor

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ProfunctorSpec extends AnyFlatSpec with should.Matchers with ProfunctorLaws {


  it should "laws" in {

  }

  it should "profunctor" in {
    val transform: Function[Double, String] = Profunctor[Function].dimap((x: Int) => x.toString)((x: Double) => x.toInt)(_ + " profunctor optics")

    transform(1.99) shouldBe "1 profunctor optics"

  }

}