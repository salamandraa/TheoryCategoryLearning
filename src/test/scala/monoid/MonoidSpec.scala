package monoid


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._
import sun.awt.SunToolkit.OperationTimedOut

class MonoidSpec extends AnyFlatSpec with should.Matchers with MonoidLaws {

  import monoid.Monoid._

  it should "laws" in {

    identityLaw(2) shouldBe true
    compositionLaw(2, 3, 4) shouldBe true

    identityLaw("hello") shouldBe true
    compositionLaw("a", "b", "c") shouldBe true

    identityLaw(Seq(1, 2)) shouldBe true
    compositionLaw(Seq(1, 2), Seq(3, 4), Seq(5)) shouldBe true

    identityLaw(Option(1)) shouldBe true
    identityLaw(Option.empty[Int]) shouldBe true
    compositionLaw(Option(1), Some(2), Some(5)) shouldBe true
    compositionLaw(Option(1), None, None) shouldBe true
    compositionLaw(Option.empty[Int], None, None) shouldBe true


    identityLaw((2, "hello")) shouldBe true
    compositionLaw((2, "a"), (3, "b"), (4, "c")) shouldBe true

    identityLaw(Map("a" -> 1, "b" -> 2, "a" -> 3)) shouldBe true
    compositionLaw(Map("a" -> 1), Map("b" -> 2, "a" -> 10), Map("a" -> 3)) shouldBe true

    identityLaw(2)(Monoid.monoidIntMultiplication) shouldBe true
    compositionLaw(2, 3, 4)(Monoid.monoidIntMultiplication) shouldBe true


    identityLaw(true)(Monoid.monoidOrBoolean) shouldBe true
    identityLaw(false)(Monoid.monoidOrBoolean) shouldBe true
    compositionLaw(true, false, false)(Monoid.monoidOrBoolean) shouldBe true
    compositionLaw(false, true, true)(Monoid.monoidOrBoolean) shouldBe true

    identityLaw(true)(Monoid.monoidAndBoolean) shouldBe true
    identityLaw(false)(Monoid.monoidAndBoolean) shouldBe true
    compositionLaw(true, false, false)(Monoid.monoidAndBoolean) shouldBe true
    compositionLaw(false, true, true)(Monoid.monoidAndBoolean) shouldBe true

    identityLawLeft((x: Int) => x + 2).apply(10) shouldBe identityLawRight((x: Int) => x + 2).apply(10)
    compositionLawLeft((x: Int) => x + 2, (x: Int) => x + 10, (x: Int) => x * 100).apply(10) shouldBe compositionLawRight((x: Int) => x + 2, (x: Int) => x + 10, (x: Int) => x * 100).apply(10)

    identityLawLeft((x: Int) => (x + 2).toString).apply(10) shouldBe identityLawRight((x: Int) => (x + 2).toString).apply(10)
    compositionLawLeft((x: Int) => (x + 2).toString, (x: Int) => (x + 10).toString, (x: Int) => (x * 100).toString).apply(10) shouldBe compositionLawRight((x: Int) => (x + 2).toString, (x: Int) => (x + 10).toString, (x: Int) => (x * 100).toString).apply(10)

  }

  it should "check diffrent types" in {
    Monoid[Int].combine(1, 2) shouldBe 3
    Monoid[String].combine("1", "2") shouldBe "12"
    Monoid[Seq[Int]].combine(Seq(1), Seq(2)) shouldBe Seq(1, 2)
    Monoid[List[Int]].combine(List(1, 2), List(3, 4)) shouldBe Seq(1, 2, 3, 4)
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

    val l1: IndexedSeq[Int] = List(1, 2, 3, 4, 5).toIndexedSeq
    l1.foldMapV(x => x) should be(15)
    l1.foldMapV(i => i.toString) should be("12345")
    l1.foldMapV(i => (i, i.toString)) should be(15 -> "12345")
    List(1, 2).toIndexedSeq.foldMapV(i => (i, i.toString)) should be(3 -> "12")

    bag(Vector("a", "rose", "is", "a", "rose")) shouldBe Map("a" -> 2, "rose" -> 2, "is" -> 1)
  }

  it should "test from https://www.scala-exercises.org/cats/monoid" in {

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
