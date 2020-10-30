


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import matchers._
import monoid.Monoid

class MonoidSpec extends AnyFlatSpec with should.Matchers {

  it should "check diffrent types" in {
    Monoid[Int].combine(1, 2) shouldBe 3
    Monoid[String].combine("1", "2") shouldBe "12"
    Monoid[Seq[Int]].combine(Seq(1), Seq(2)) shouldBe Seq(1, 2)
    Monoid[Seq[Seq[Int]]].combine(Seq(Seq(1), Seq(3)), Seq(Seq(2))) shouldBe Seq(Seq(1), Seq(3), Seq(2))
    Monoid[Option[Int]].combine(Some(1), Some(2)) shouldBe Some(3)
    Monoid[Option[Int]].combine(Some(1), None) shouldBe Some(1)
    Monoid[Option[Int]].combine(None, Some(2)) shouldBe Some(2)
    Monoid[Option[Int]].combine(None, None) shouldBe None
    Monoid[Seq[Option[Int]]].combine(Seq(Some(1)), Seq(Some(2))) shouldBe Seq(Some(1), Some(2))
    Monoid[(Int, String)].combine(1 -> "1", 2 -> "2") shouldBe 3 -> "12"
    Monoid[Seq[(Int, String)]].combine(Seq(1 -> "1"), Seq(2 -> "2")) shouldBe Seq(1 -> "1", 2 -> "2")
    Monoid.combineAll(Seq("1", "2", "3")) shouldBe "123"
    Monoid.combineAll(Seq(1, 2, 3)) shouldBe 6
  }

  it should "test from https://www.scala-exercises.org/cats/monoid" in {
    import monoid.Monoid._

    Monoid[String].empty should be("")
    Monoid[String].combineAll(List("a", "b", "c")) should be("abc")
    Monoid[String].combineAll(List()) should be("")

    Monoid[Map[String, Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3))) should be(Map("a" -> 4, "b" -> 2))
    Monoid[Map[String, Int]].combineAll(List()) should be(Map())

    val l1 = List(1, 2, 3, 4, 5)
    l1.foldMap(x => x) should be(15)
    l1.foldMap(i => i.toString) should be("12345")

    val l = List(1, 2, 3, 4, 5)
    l.foldMap(i => (i, i.toString)) should be(15 -> "12345")
  }

}
