package functor


import data.Id.Id
import data.{Const, Reader}
import functor.CovariantFunctor
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._

class CovariantFunctorSpec extends AnyFlatSpec with should.Matchers {

  it should "check diffrent types" in {
    import functor.CovariantFunctor._
    CovariantFunctor[Seq].map(Seq("Hello", ""))(_.length) should be(Seq(5, 0))
    CovariantFunctor[Seq].map(List("Hello", ""))(_.length) should be(List(5, 0))
    CovariantFunctor[List].map(List("Hello", ""))(_.length) should be(List(5, 0))
    CovariantFunctor[List].map(List("left" -> "right"))(_._2) should be(List("right"))
  }

  it should "test from https://www.scala-exercises.org/cats/functor" in {
    import functor.CovariantFunctor._

    CovariantFunctor[Option].map(Option("Hello"))(_.length) should be(Option(5))
    CovariantFunctor[Option].map(None: Option[String])(_.length) should be(None)


    val lenOption: Option[String] => Option[Int] = CovariantFunctor[Option].lift(_.length)
    lenOption(Some("Hello")) should be(Some(5))
    lenOption(None) should be(None)

    val source = List("Cats", "is", "awesome")
    val product = CovariantFunctor[List].fproduct(source)(_.length).toMap


    product.get("Cats").getOrElse(0) should be(4)
    product.get("is").getOrElse(0) should be(2)
    product.get("awesome").getOrElse(0) should be(7)

    val listOpt: CovariantFunctor[λ[α => List[Option[α]]]] = CovariantFunctor[List] compose CovariantFunctor[Option]
    listOpt.map(List(Some(1), None, Some(3)))(_ + 1) should be(List(Some(2), None, Some(4)))
  }

  it should "bifunctor test" in {
    import Bifunctor._
    Bifunctor[Tuple2].bimap(1, "1")(_.toString, _.toInt) should be(("1", 1))
    Bifunctor[Map].bimap(Map((1, "1")))(_.toString, _.toInt) should be(Map(("1", 1)))
    Bifunctor[Either].bimap(Left[Int, String](1))(_.toString, _.toInt) should be(Left("1"))
    Bifunctor[Either].bimap(Right[Int, String]("1"))(_.toString, _.toInt) should be(Right(1))
    Bifunctor[Tuple2].compose[Either].bimap((Left(1), Right("1")))(_.toString, _.toInt) shouldBe(Left("1"), Right(1))
    Bifunctor[Either].compose[Tuple2].bimap(Left((1, "1")))(_.toString, _.toInt) shouldBe Left(("1", 1))
    Bifunctor[Tuple2].compose[Tuple2].bimap(((1, "2"), (3, "4")))(_.toString, _.toInt) shouldBe(("1", 2), ("3", 4))
    val leftFunctor: CovariantFunctor[(*, String)] = Bifunctor[Tuple2].leftFunctor
    leftFunctor.map((1, "2"))(_ - 1) shouldBe(0, "2")
    val rightFunctor: CovariantFunctor[(Int, *)] = Bifunctor[Tuple2].rightFunctor
    rightFunctor.map((1, "2"))(_.toInt) shouldBe ((1, 2))
  }

  it should "functor reader test" in {
    import CovariantFunctor._

    val fun: Int => String = CovariantFunctor[Int => *].map(_ + 1)(_.toString)
    fun(2) shouldBe "3"

    val fun2: Reader[Int, String] = CovariantFunctor[Reader[Int, *]].map(Reader(_ + 1))(_.toString)
    fun2(2) shouldBe "3"
  }

  it should "functor Const" in {
    CovariantFunctor[Const[Int, *]].map(Const(10))(_ => "") shouldBe Const(10)
    CovariantFunctor[Const[Unit, *]].map(Const(()))(_ => "") shouldBe Const(())
  }

  it should "functor Id" in {
    val idInt: Id[Int] = 10
    CovariantFunctor.apply(CovariantFunctor[Id]).map(idInt)(_ + 1) shouldBe 11
    CovariantFunctor.apply(CovariantFunctor[Id]).map(10)(_ + 1) shouldBe 11
  }


}
