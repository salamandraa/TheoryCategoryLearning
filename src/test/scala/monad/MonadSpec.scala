package monad

import data.{Reader, Tree}
import monoid.Monoid
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.annotation.tailrec

class MonadSpec extends AnyFlatSpec with should.Matchers {


  it should "laws" in {

  }

  it should "test https://www.scala-exercises.org/cats/monad" in {
    Monad[Option].pure(42) shouldBe Some(42)

    Monad[List].flatMap(List(1, 2, 3))(x => List(x, x)) shouldBe List(1, 1, 2, 2, 3, 3)
    //    Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")) should be()
    //    Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) should be()
    //    optionTMonad[List].pure(42) should be(OptionT())


    Monad[Option].traverse(List(1, 2, 3))(Some(_)) shouldBe Some(List(1, 2, 3))
    Monad[Option].traverse(List(1, 2, 3))(x => if (x == 1) None else Some(x)) shouldBe None
    Monad[Option].traverse(List(1, 2, 3))(x => if (x == 2) None else Some(x)) shouldBe None
    Monad[Option].traverse(List(1, 2, 3))(x => if (x == 3) None else Some(x)) shouldBe None
    Monad[Option].traverse(List(1, 2, 3))(x => if (x == 4) None else Some(x)) shouldBe Some(List(1, 2, 3))

    Monad[Option].sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    Monad[Option].sequence(List(None, Some(2), Some(3))) shouldBe None
    Monad[Option].sequence(List(Some(1), None, Some(3))) shouldBe None
    Monad[Option].sequence(List(Some(1), Some(2), None)) shouldBe None
    Monad[Option].sequence(List(None, None, None)) shouldBe None

    Monoid.combineAll(List(Option(1), Some(2), Some(3))) shouldBe Some(6)
    Monoid.combineAll(List(None, Some(2), Some(3))) shouldBe Some(5)
    Monoid.combineAll(List(Some(1), None, Some(3))) shouldBe Some(4)
    Monoid.combineAll(List(Some(1), Some(2), None)) shouldBe Some(3)
    Monoid.combineAll(List(Option.empty[Int], None, None)) shouldBe None

  }

  it should "test" in {
    val optionMonad = new cats.Monad[Option] {
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f

      @tailrec
      override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
        case None => None
        case Some(Left(a)) => tailRecM(a)(f)
        case Some(Right(b)) => Some(b)
      }

      override def pure[A](x: A): Option[A] = Some(x)
    }

    import data.Tree
    import data.Leaf
    import data.Branch
    implicit val treeMonad: cats.Monad[Tree] = new cats.Monad[Tree] {
      override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
        case Leaf(value) => f(value)
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      }


      override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = ???

      override def pure[A](x: A): Tree[A] = Leaf(x)
    }
  }

  it should "EXERCISE 11.7 Implement the Kleisli composition function compose ." in {

    def compose[A, B, C, F[_] : Monad](f: A => F[B], g: B => F[C]): A => F[C] = (a: A) => implicitly[Monad[F]].flatMap(f(a))(g)

    val f: Int => Option[Int] = (x: Int) => Some(x)
    val g: Int => Option[Int] = (x: Int) => Some(10 * x)
    val h: Int => Option[Int] = (x: Int) => Some(100 * x)
    compose(compose(f, g), h) == compose(f, compose(g, h))

    //    EXERCISE 11.8
    //    compose and unit .
    //    def flatMap[A, B, M[_]:Monad](ma: M[A])(f: A => M[B]): M[B] = compose((x:A) => implicitly[Monad[F]].pure(ma,f)

    //    EXERCISE 11.12
    //    There’s a third minimal set of monadic combinators: map , unit , and join . Implement
    //    join in terms of flatMap .
    def join[A, F[_] : Monad](mma: F[F[A]]): F[A] = implicitly[Monad[F]].flatMap(mma)(identity)

    join(Option(Option(1))) shouldBe Option(1)
    join(Option(Option.empty[Int])) shouldBe None
    join(Option.empty[Option[Int]]) shouldBe None


    //    EXERCISE 11.13
    //    Implement either flatMap or compose in terms of join and map .

    def flatMap[A, B, M[_] : Monad](ma: M[A])(f: A => M[B]): M[B] = implicitly[Monad[M]].flatten(implicitly[Monad[M]].map(ma)(f))

    flatMap(Option(1))(x => Some(x + 1)) shouldBe Option(2)
    flatMap(Option.empty[Int])(x => Some(x + 1)) shouldBe None

    Monad[Reader[Int, *]].flatMap(Reader((x: Int) => x + 3))(a => Reader((x: Int) => (x * a).toString)).apply(10) shouldBe "130"

    cats.data.Reader((x: Int) => x + 3).flatMap(a => cats.data.Reader((x: Int) => (x * a).toString)).apply(10) shouldBe "130"

  }

}