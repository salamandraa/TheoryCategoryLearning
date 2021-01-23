


import data.Kleisli
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import matchers._


//import cats.Monoid
//import cats.implicits._

class KleisliSpec extends AnyFlatSpec with should.Matchers {

  it should "test" in {

    val negate_1: Boolean => (Boolean, String) = { x: Boolean =>
      (!x, s"negate_1 called with ${x}\n")
    }

    val negate_2: Boolean => (Boolean, String) = { x: Boolean =>
      (!x, s"negate_2 called with ${x}\n")
    }

    Kleisli.empty { _: Unit => 10 }.map(_ + 1).run(()) shouldBe(11, "")

    Kleisli(negate_1).flatMap(_ => Kleisli(negate_2)).run(true) shouldBe(true, "negate_1 called with true\nnegate_2 called with false\n")
  }
}
