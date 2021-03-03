package monada

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MonadaSpec extends AnyFlatSpec with should.Matchers {


  it should "laws" in {

  }

  it should "test https://www.scala-exercises.org/cats/monad" in {
    Monad[Option].pure(42) shouldBe Some(42)

    Monad[List].flatMap(List(1, 2, 3))(x => List(x, x)) shouldBe List(1, 1, 2, 2, 3, 3)
    //    Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")) should be()
    //    Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) should be()
    //    optionTMonad[List].pure(42) should be(OptionT())
  }

}