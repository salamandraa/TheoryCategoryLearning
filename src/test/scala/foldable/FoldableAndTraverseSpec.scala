package foldable

import cats.data
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Try

class FoldableAndTraverseSpec extends AnyFlatSpec with should.Matchers {


  it should "test" in {
    List(1, 2, 3).foldLeft(List.empty[Int])((list, a) => a :: list) shouldBe List(3, 2, 1)
    List(1, 2, 3).foldRight(List.empty[Int])((a, list) => a :: list) shouldBe List(1, 2, 3)


    //    Prove this to yourself by implementing substitutes for List's map , flatMap ,
    //    filter , and sum methods in terms of foldRight .
    trait FoldableForExersize[F[_]] {
      def foldRight[A, B](fa: F[A], zero: B)(f: (A, B) => B): B

      def foldLeft[A, B](fa: F[A], zero: B)(f: (B, A) => B): B

      def map[A, B](fa: F[A])(f: A => B): F[B]

      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

      def filter[A](fa: F[A])(f: A => Boolean): F[A]

      def sum[A](fa: List[A])(merge: (A, A) => A): Option[A]
    }

    val foldableListMy: FoldableForExersize[List] = new FoldableForExersize[List] {
      override def foldRight[A, B](fa: List[A], zero: B)(f: (A, B) => B): B = fa.foldRight(zero)(f)

      override def foldLeft[A, B](fa: List[A], zero: B)(f: (B, A) => B): B = fa.foldLeft(zero)(f)

      override def map[A, B](fa: List[A])(f: A => B): List[B] = foldRight(fa, List.empty[B])((e, list) => f(e) :: list)

      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = foldRight(fa, List.empty[B])((e, list) => f(e) ++ list)


      override def filter[A](fa: List[A])(f: A => Boolean): List[A] = foldRight(fa, List.empty[A])((e, list) => if (f(e)) e :: list else list)


      override def sum[A](fa: List[A])(merge: (A, A) => A): Option[A] = foldRight(fa, Option.empty[A]) { (e, acc) =>
        acc match {
          case Some(sumValue) => Some(merge(e, sumValue))
          case None => Some(e)
        }
      }
    }

    foldableListMy.map(List(1, 2, 3))(_ + 1) shouldBe List(2, 3, 4)
    foldableListMy.flatMap(List(1, 2, 3))(x => List(x, x + 1)) shouldBe List(1, 2, 2, 3, 3, 4)
    foldableListMy.filter(List(1, 2, 3))(_ != 2) shouldBe List(1, 3)
    foldableListMy.sum(List(1, 2, 3))(_ + _) shouldBe Some(6)


    import cats.Foldable
    val ints = List(1, 2, 3)
    Foldable[List].foldLeft(ints, 0)(_ + _) shouldBe 6
    val maybeInt = Option(123)
    Foldable[Option].foldLeft(maybeInt, 10)(_ * _) shouldBe 1230

    import cats.Eval
    import cats.Foldable
    def bigData = (1 to 100000).to(LazyList)

    val eval: Eval[Long] = Foldable[LazyList].foldRight(bigData, Eval.now(0L)) { (num, eval) =>
      eval.map(_ + num)
    }
    eval.value shouldBe 5000050000L

    Foldable[Option].nonEmpty(Option(42)) shouldBe true

    Foldable[List].find(List(1, 2, 3))(_ % 2 == 0) shouldBe Some(2)

    Foldable[List].combineAll(List(1, 2, 3)) shouldBe 6


    Foldable[List].foldMap(List(1, 2, 3))(_.toString) shouldBe "123"


    val ints2 = List(Vector(1, 2, 3), Vector(4, 5, 6))
    (Foldable[List] compose Foldable[Vector]).combineAll(ints2) shouldBe 21

    import cats.syntax.foldable._

    List(1, 2, 3).combineAll shouldBe 6
    // res12: Int = 6
    List(1, 2, 3).foldMap(_.toString) shouldBe "123"

    //explicit
    List(1, 2, 3).foldLeft(0)(_ + _)

    //implicit
    def sum[F[_] : Foldable](values: F[Int]): Int =
      values.foldLeft(0)(_ + _)

    import cats.instances.vector._ // for Applicative

    import cats.Applicative
    import cats.syntax.applicative._
    import cats.syntax.apply._

    def listTraverse[F[_] : Applicative, A, B]
    (list: List[A])(func: A => F[B]): F[List[B]] =
      list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
        (accum, func(item)).mapN(_ :+ _)
      }

    def listSequence[F[_] : Applicative, B]
    (list: List[F[B]]): F[List[B]] =
      listTraverse(list)(identity)

    listSequence(List(Vector(1, 2), Vector(3, 4))) shouldBe Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))

    listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) shouldBe Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))


    import cats.instances.option._ // for Applicative
    def process(inputs: List[Int]) =
      listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

    process(List(2, 4, 6)) shouldBe Some(List(2, 4, 6))
    process(List(1, 2, 3)) shouldBe None


    import cats.data.Validated
    import cats.instances.list._ // for Monoid
    type ErrorsOr[A] = Validated[List[String], A]

    def process2(inputs: List[Int]): ErrorsOr[List[Int]] =
      listTraverse(inputs) { n =>
        if (n % 2 == 0) {
          Validated.valid(n)
        } else {
          Validated.invalid(List(s"$n is not even"))
        }
      }

    process2(List(2, 4, 6)) shouldBe Validated.valid(List(2, 4, 6))
    process2(List(1, 2, 3)) shouldBe Validated.invalid(List("1 is not even", "3 is not even"))

    import cats.syntax.traverse._


    List(Vector(1, 2), Vector(3, 4)).sequence shouldBe Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))

  }
}
