package monoid


import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._


//import cats.Monoid
//import cats.implicits._

class SemigroupSpec extends AnyFlatSpec with should.Matchers {

  import monoid.Semigroup._


  it should "test" in {
    val semigroupInt2: Semigroup[Int] = semigroupString.imap(_.toInt, (x: Int) => x.toString)
    semigroupInt2.combine(10, 20) shouldBe 1020

  }
  it should "test from https://www.scala-exercises.org/cats/semigroup" in {

    Semigroup[Int].combine(1, 2) should be(3)
    Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) should be(List(1, 2, 3, 4, 5, 6))
    Semigroup[Option[Int]].combine(Option(1), Option(2)) should be(Option(3))
    Semigroup[Option[Int]].combine(Option(1), None) should be(Option(1))


    Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6) should be(70)


    val aMap = Map("foo" -> Map("bar" -> 5))
    val anotherMap = Map("foo" -> Map("bar" -> 6))
    val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)
    combinedMap.get("foo") should be(Some(Map("bar" -> 11)))


    val one: Option[Int] = Option(1)
    val two: Option[Int] = Option(2)
    val n: Option[Int] = None

    one |+| two should be(Option(3))
    n |+| two should be(Option(2))
    n |+| n should be(None)
    two |+| n should be(Option(2))
  }
}
