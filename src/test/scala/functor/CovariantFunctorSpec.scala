package functor


import data.Id.Id
import data.{Const, Reader}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._

import scala.util.Try

class CovariantFunctorSpec extends AnyFlatSpec with should.Matchers with FunctorCheck {

  it should "rules" in {

    import data.Id.Id
    identityFunctor[Id, Int](10) shouldBe true
    compositionFunctor[Id, Int, Double, String](10)(_.toDouble)(_.toString) shouldBe true

    identityFunctor(List(10, 20)) shouldBe true
    identityFunctor(List.empty[Int]) shouldBe true
    compositionFunctor(List(10, 20))(_.toDouble)(_.toString) shouldBe true
    compositionFunctor(List.empty[Int])(_.toDouble)(_.toString) shouldBe true

    identityFunctor(Seq(10, 20)) shouldBe true
    identityFunctor(Seq.empty[Int]) shouldBe true
    compositionFunctor(Seq(10, 20))(_.toDouble)(_.toString) shouldBe true
    compositionFunctor(Seq.empty[Int])(_.toDouble)(_.toString) shouldBe true

    identityFunctor(Option(10)) shouldBe true
    identityFunctor(Option.empty[Int]) shouldBe true
    compositionFunctor(Option(10))(_.toDouble)(_.toString) shouldBe true
    compositionFunctor(Option.empty[Int])(_.toDouble)(_.toString) shouldBe true

    identityFunctor(Set(10, 20)) shouldBe true
    identityFunctor(Set.empty[Int]) shouldBe true
    compositionFunctor(Set(10, 20))(_.toDouble)(_.toString) shouldBe true
    compositionFunctor(Set.empty[Int])(_.toDouble)(_.toString) shouldBe true

    Try(10 / 0).isFailure shouldBe true
    identityFunctor(Try(10)) shouldBe true
    identityFunctor(Try(10 / 0)) shouldBe true
    compositionFunctor(Try(10))(_.toDouble)(_.toString) shouldBe true
    compositionFunctor(Try(10 / 0))(_.toDouble)(_.toString) shouldBe true
  }

  it should "check diffrent types" in {
    CovariantFunctor[Seq].map(Seq("Hello", ""))(_.length) should be(Seq(5, 0))
    CovariantFunctor[Seq].map(List("Hello", ""))(_.length) should be(List(5, 0))
    CovariantFunctor[List].map(List("Hello", ""))(_.length) should be(List(5, 0))
    CovariantFunctor[List].map(List("left" -> "right"))(_._2) should be(List("right"))
  }

  it should "test from https://www.scala-exercises.org/cats/functor" in {

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


  it should "functor reader test" in {

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
