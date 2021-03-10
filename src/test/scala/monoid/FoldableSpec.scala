package monoid


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._

class FoldableSpec extends AnyFlatSpec with should.Matchers {

  import monoid.Foldable._


  it should "check diffrent types" in {
    //    Monoid[Int].combine(1, 2) shouldBe 3
    //    Monoid[String].combine("1", "2") shouldBe "12"
    //    Monoid[Seq[Int]].combine(Seq(1), Seq(2)) shouldBe Seq(1, 2)
    //    Monoid[List[Int]].combine(List(1, 2), List(3, 4)) shouldBe Seq(1, 2, 3, 4)
    //    Monoid[Seq[Seq[Int]]].combine(Seq(Seq(1), Seq(3)), Seq(Seq(2))) shouldBe Seq(Seq(1), Seq(3), Seq(2))
    //    Monoid[Option[Int]].combine(Some(1), Some(2)) shouldBe Some(3)
    //    Monoid[Option[Int]].combine(Some(1), None) shouldBe Some(1)
    //    Monoid[Option[Int]].combine(None, Some(2)) shouldBe Some(2)
    //    Monoid[Option[Int]].combine(None, None) shouldBe None
    //    Monoid[Seq[Option[Int]]].combine(Seq(Some(1)), Seq(Some(2))) shouldBe Seq(Some(1), Some(2))
    //    Monoid[(Int, String)].combine(1 -> "1", 2 -> "2") shouldBe 3 -> "12"
    //    Monoid[Seq[(Int, String)]].combine(Seq(1 -> "1"), Seq(2 -> "2")) shouldBe Seq(1 -> "1", 2 -> "2")
    //    Monoid.combineAll(Seq("1", "2", "3")) shouldBe "123"
    //    Monoid.combineAll(Seq(1, 2, 3)) shouldBe 6
    //
    //    val l1: IndexedSeq[Int] = List(1, 2, 3, 4, 5).toIndexedSeq
    //    l1.foldMapV(x => x) should be(15)
    //    l1.foldMapV(i => i.toString) should be("12345")
    //    l1.foldMapV(i => (i, i.toString)) should be(15 -> "12345")
    //    List(1, 2).toIndexedSeq.foldMapV(i => (i, i.toString)) should be(3 -> "12")
    //
    //    bag(Vector("a", "rose", "is", "a", "rose")) shouldBe Map("a" -> 2, "rose" -> 2, "is" -> 1)
  }

  it should "test from https://www.scala-exercises.org/cats/foldable" in {
    Foldable[List].foldRight(List(1, 2, 3), 0)(_ + _) should be(6)
    Foldable[List].foldRight(List("a", "b", "c"), "")(_ + _) should be("abc")
    Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) should be(6)
    Foldable[List].foldLeft(List("a", "b", "c"), "")(_ + _) should be("abc")


    //    val lazyResult = Foldable[List].foldRight(List(1, 2, 3), Now(0))((x, rest) => Later(x + rest.value))
    //    lazyResult.value should be()

    Foldable[List].fold(List("a", "b", "c")) should be("abc")
    Foldable[List].fold(List(1, 2, 3)) should be(6)

    Foldable[List].foldMap(List("a", "b", "c"))(_.length) should be(3)
    Foldable[List].foldMap(List(1, 2, 3))(_.toString) should be("123")

    //    def foldK[G[_], A](fga: F[G[A]])(implicit G: MonoidK[G]): G[A] =
    //        cats.Foldable[List].foldK()
    //    Foldable[List].foldK(List(List(1, 2), List(3, 4, 5))) should be()
    //    Foldable[List].foldK(List(None, Option("two"), Option("three"))) should be()

    Foldable[List].find(List(1, 2, 3))(_ > 2) should be(Some(3))
    Foldable[List].find(List(1, 2, 3))(_ > 5) should be(None)

    Foldable[List].exists(List(1, 2, 3))(_ > 2) should be(true)
    Foldable[List].exists(List(1, 2, 3))(_ > 5) should be(false)

    Foldable[List].forall(List(1, 2, 3))(_ <= 3) should be(true)
    Foldable[List].forall(List(1, 2, 3))(_ < 3) should be(false)

    Foldable[List].toList(List(1, 2, 3)) should be(List(1, 2, 3))
    Foldable[Option].toList(Option(42)) should be(List(42))
    Foldable[Option].toList(None) should be(Nil)

    Foldable[List].filter_(List(1, 2, 3))(_ < 3) should be(List(1, 2))
    Foldable[Option].filter_(Option(42))(_ != 42) should be(Nil)

    //    def parseInt(s: String): Option[Int] =
    //      Either.catchOnly[NumberFormatException](s.toInt).toOption
    //    traverse the foldable mapping A values to G[B], and combining them using Applicative[G] and discarding the results.
    //    (implicit G: Applicative[G])
    //      This method is primarily useful when G[_] represents an action or effect, and the specific B aspect of G[B] is not otherwise needed. The B will be discarded and Unit returned instead.
    //              cats.Foldable[List].traverse_(List("1", "2", "3"))(parseInt) should be()
    //    Foldable[List].traverse_(List("a", "b", "c"))(parseInt) should be()


    //        val FoldableListOption = cats.Foldable[List].compose[Option]
    //        FoldableListOption.fold(List(Option(1), Option(2), Option(3), Option(4))) should be()
    //        FoldableListOption.fold(List(Option("1"), Option("2"), None, Option("3"))) should be()

    //

    Foldable[List].isEmpty(List(1, 2, 3)) should be(false)
    Foldable[List].isEmpty(List()) should be(true)
    //        Foldable[List].dropWhile_(List(1, 2, 3))(_ < 2) should be()
    //        Foldable[List].takeWhile_(List(1, 2, 3))(_ < 2) should be()
  }

}
