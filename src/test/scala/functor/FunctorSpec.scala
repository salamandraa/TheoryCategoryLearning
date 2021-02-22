package functor


import functor.Functor
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._

class FunctorSpec extends AnyFlatSpec with should.Matchers {

  it should "check diffrent types" in {
    import functor.Functor._
    Functor[Seq].map(Seq("Hello", ""))(_.length) should be(Seq(5, 0))
    Functor[Seq].map(List("Hello", ""))(_.length) should be(List(5, 0))
    Functor[List].map(List("Hello", ""))(_.length) should be(List(5, 0))
    Functor[List].map(List("left" -> "right"))(_._2) should be(List("right"))
  }

  it should "test from https://www.scala-exercises.org/cats/functor" in {
    import functor.Functor._

    Functor[Option].map(Option("Hello"))(_.length) should be(Option(5))
    Functor[Option].map(None: Option[String])(_.length) should be(None)


    val lenOption: Option[String] => Option[Int] = Functor[Option].lift(_.length)
    lenOption(Some("Hello")) should be(Some(5))
    lenOption(None) should be(None)

    val source = List("Cats", "is", "awesome")
    val product = Functor[List].fproduct(source)(_.length).toMap


    product.get("Cats").getOrElse(0) should be(4)
    product.get("is").getOrElse(0) should be(2)
    product.get("awesome").getOrElse(0) should be(7)

    val listOpt: Functor[λ[α => List[Option[α]]]] = Functor[List] compose Functor[Option]
    listOpt.map(List(Some(1), None, Some(3)))(_ + 1) should be(List(Some(2), None, Some(4)))
  }

  it should "bifunctor test" in {
    import Bifunctor._
    Bifunctor[Tuple2].bimap(1, "1")(_.toString, _.toInt) should be(("1", 1))
    Bifunctor[Map].bimap(Map((1, "1")))(_.toString, _.toInt) should be(Map(("1", 1)))
    Bifunctor[Either].bimap(Left[Int, String](1))(_.toString, _.toInt) should be(Left("1"))
    Bifunctor[Either].bimap(Right[Int, String]("1"))(_.toString, _.toInt) should be(Right(1))
  }

}
