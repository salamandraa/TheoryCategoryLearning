package functor

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._

class BifunctorSpec extends AnyFlatSpec with should.Matchers {

  it should "bifunctor test" in {
    Bifunctor[Tuple2].bimap(1, "1")(_.toString, _.toInt) should be(("1", 1))
    Bifunctor[Map].bimap(Map((1, "1")))(_.toString, _.toInt) should be(Map(("1", 1)))
    Bifunctor[Either].bimap(Left[Int, String](1))(_.toString, _.toInt) should be(Left("1"))
    Bifunctor[Either].bimap(Right[Int, String]("1"))(_.toString, _.toInt) should be(Right(1))
    Bifunctor[Tuple2].compose[Either].bimap((Left(1), Right("1")))(_.toString, _.toInt) shouldBe(Left("1"), Right(1))
    Bifunctor[Either].compose[Tuple2].bimap(Left((1, "1")))(_.toString, _.toInt) shouldBe Left(("1", 1))
    Bifunctor[Tuple2].compose[Tuple2].bimap(((1, "2"), (3, "4")))(_.toString, _.toInt) shouldBe(("1", 2), ("3", 4))
    val leftFunctor: Functor[(*, String)] = Bifunctor[Tuple2].leftFunctor
    leftFunctor.map((1, "2"))(_ - 1) shouldBe(0, "2")
    val rightFunctor: Functor[(Int, *)] = Bifunctor[Tuple2].rightFunctor
    rightFunctor.map((1, "2"))(_.toInt) shouldBe ((1, 2))
  }
}
