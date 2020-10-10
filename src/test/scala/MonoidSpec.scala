


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
}
