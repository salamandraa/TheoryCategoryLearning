package monada

import monoid.Monoid
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MonadSpec extends AnyFlatSpec with should.Matchers {


  it should "laws" in {

  }

  it should "test https://www.scala-exercises.org/cats/monad" in {
    Monad[Option].pure(42) shouldBe Some(42)

    Monad[List].flatMap(List(1, 2, 3))(x => List(x, x)) shouldBe List(1, 1, 2, 2, 3, 3)
    //    Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")) should be()
    //    Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) should be()
    //    optionTMonad[List].pure(42) should be(OptionT())


    Monad[Option].sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    Monad[Option].sequence(List(None, Some(2), Some(3))) shouldBe None
    Monad[Option].sequence(List(Some(1), None, Some(3))) shouldBe None
    Monad[Option].sequence(List(Some(1), Some(2), None)) shouldBe None
    Monad[Option].sequence(List(None, None, None)) shouldBe None

    Monad[Option].traverse(List(1, 2, 3))(Some(_)) shouldBe Some(List(1, 2, 3))
    Monad[Option].traverse(List(1, 2, 3))(x => if (x == 1) None else Some(x)) shouldBe None
    Monad[Option].traverse(List(1, 2, 3))(x => if (x == 2) None else Some(x)) shouldBe None
    Monad[Option].traverse(List(1, 2, 3))(x => if (x == 3) None else Some(x)) shouldBe None
    Monad[Option].traverse(List(1, 2, 3))(x => if (x == 4) None else Some(x)) shouldBe Some(List(1, 2, 3))

    Monoid.combineAll(List(Option(1), Some(2), Some(3))) shouldBe Some(6)
    Monoid.combineAll(List(None, Some(2), Some(3))) shouldBe Some(5)
    Monoid.combineAll(List(Some(1), None, Some(3))) shouldBe Some(4)
    Monoid.combineAll(List(Some(1), Some(2), None)) shouldBe Some(3)
    Monoid.combineAll(List(Option.empty[Int], None, None)) shouldBe None

  }

}