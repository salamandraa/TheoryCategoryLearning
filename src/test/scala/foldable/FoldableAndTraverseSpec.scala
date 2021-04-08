package foldable

import cats.{Applicative, Eval, Id, Semigroupal, Traverse, data}
import data.NonEmptyList
import monad.Monad
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

  it should "Traversable functors and Traverse" in {

    import data1.Validation

    val validateError1: Validation[String, Int] = Validation.Failure(NonEmptyList("Error1", Nil))
    val validateError23: Validation[String, Int] = Validation.Failure(NonEmptyList("Error2", "Error3" :: Nil))
    val validateOk10: Validation[String, Int] = Validation.Success(10)
    val validateOk20: Validation[String, Int] = Validation.Success(20)

    Traverse[List].sequence(List(validateOk10, validateError1, validateError23)) shouldBe Validation.Failure(NonEmptyList("Error1", "Error2" :: "Error3" :: Nil))
    Traverse[List].sequence(List(validateOk10)) shouldBe Validation.Success(List(10))
    Traverse[List].sequence(List.empty[Validation[String, Int]]) shouldBe Validation.Success(Nil)

    import cats.syntax.apply._

    //EXERCISE 12.12
    def sequenceMap[K, V, F[_] : Applicative](ofa: Map[K, F[V]]): F[Map[K, V]] = {
      val applicative: Applicative[F] = implicitly[Applicative[F]]
      ofa.foldLeft(applicative.pure(Map.empty[K, V])) { case (acc, (k, fV)) =>
        (acc, fV).mapN((mapKV, v) => mapKV + (k -> v))
      }
    }

    val mapKV: Map[Int, Validation[String, Int]] = Map(1 -> validateError1, 2 -> validateError23, 5 -> validateOk10, 13 -> validateOk20)

    sequenceMap(mapKV) shouldBe Validation.Failure(NonEmptyList("Error1", "Error2" :: "Error3" :: Nil))
    sequenceMap(Map(5 -> validateOk10, 13 -> validateOk20)) shouldBe Validation.Success(Map(5 -> 10, 13 -> 20))


    //    EXERCISE 12.13
    //    Write Traverse instances for List , Option , and Tree .
    import cats.syntax.applicative._
    val traverseList1 = new Traverse[List] {
      override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[List[B]] = {
        foldLeft(fa, List.empty[B].pure[G]) { case (acc, x) =>
          (acc, f(x)).mapN((list, last) => list :+ last)
        }
      }

      override def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: List[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldRight(lb)(f)
    }

    traverseList1.sequence(List(validateOk10, validateOk20, validateError1, validateError23)) shouldBe Validation.Failure(NonEmptyList("Error1", "Error2" :: "Error3" :: Nil))
    traverseList1.sequence(List(validateOk10, validateOk20)) shouldBe Validation.Success(List(10, 20))
    traverseList1.sequence(List.empty[Validation[String, Int]]) shouldBe Validation.Success(Nil)

    val traverseOption1 = new Traverse[Option] {
      override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[Option[B]] = {
        //        foldLeft(fa, Option.empty[B].pure[G]) { case (acc, x) =>
        //          (acc, f(x)).mapN { (_, b) => Some(b)
        //          }
        //        }
        import cats.syntax.functor._
        fa.map(f) match {
          case Some(gb) => gb.map(Some(_))
          case None => Option.empty[B].pure[G]
        }
      }

      override def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: Option[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldRight(lb)(f)
    }

    traverseOption1.sequence(Some(validateError1)) shouldBe Validation.Failure(NonEmptyList("Error1", Nil))
    traverseOption1.sequence(Some(validateOk10)) shouldBe Validation.Success(Some(10))
    traverseOption1.sequence(Option.empty[Validation[String, Int]]) shouldBe Validation.Success(None)


    case class MultiTree[+A](head: A, tail: List[MultiTree[A]])

    def union[A](tree: MultiTree[A], last: A): MultiTree[A] = MultiTree(tree.head, tree.tail :+ MultiTree(last, Nil))


    val traverseMultiTree = new Traverse[MultiTree] {
      override def traverse[G[_], A, B](fa: MultiTree[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[MultiTree[B]] = {
        //        val res = foldLeft(fa, MultiTree(null.asInstanceOf[B], Nil).pure[G]) { case (acc, x) =>
        //          (acc, f(x)).mapN { (multiTree, last) => union(multiTree, last)
        //          }
        //        }
        //        res
        (f(fa.head), Traverse[List].traverse(fa.tail)(tree => traverse(tree)(f))).mapN((head, tail) => MultiTree(head, tail))
      }

      override def foldLeft[A, B](fa: MultiTree[A], b: B)(f: (B, A) => B): B = { // its good????
        val a = fa.head
        fa.tail match {
          case Nil => f(b, a)
          case headTree :: tailTrees => tailTrees.foldLeft(f(foldLeft(headTree, b)(f), a)) {
            case (acc, multiTree) => foldLeft(multiTree, acc)(f)
          }
        }
      }

      override def foldRight[A, B](fa: MultiTree[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
    }

    val multiTrees = MultiTree(List(0, 10), List(MultiTree(List(20, 30), Nil), MultiTree((List(40, 50, 60)), Nil)))
    traverseMultiTree.sequence(multiTrees) shouldBe List(MultiTree(0, List(MultiTree(20, List()), MultiTree(40, List()))), MultiTree(0, List(MultiTree(20, List()), MultiTree(50, List()))), MultiTree(0, List(MultiTree(20, List()), MultiTree(60, List()))), MultiTree(0, List(MultiTree(30, List()), MultiTree(40, List()))), MultiTree(0, List(MultiTree(30, List()), MultiTree(50, List()))), MultiTree(0, List(MultiTree(30, List()), MultiTree(60, List()))), MultiTree(10, List(MultiTree(20, List()), MultiTree(40, List()))), MultiTree(10, List(MultiTree(20, List()), MultiTree(50, List()))), MultiTree(10, List(MultiTree(20, List()), MultiTree(60, List()))), MultiTree(10, List(MultiTree(30, List()), MultiTree(40, List()))), MultiTree(10, List(MultiTree(30, List()), MultiTree(50, List()))), MultiTree(10, List(MultiTree(30, List()), MultiTree(60, List()))))

    //    EXERCISE 12.14
    def map[F[_] : Traverse, A, B](fa: F[A])(f: A => B): F[B] = {
      val fun: A => Id[B] = f
      Traverse[F].traverse(fa)(fun)
    }

    //  EXERCISE 12.18
    def fuse[G[_], H[_], A, B, F[_] : Traverse](fa: F[A])(f: A => G[B], g: A => H[B])
                                               (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
      Traverse[F].traverse(fa)(f) -> Traverse[F].traverse(fa)(g)
    }

    trait TraverseCompose[F[_], R[_]] extends Traverse[λ[α => F[R[α]]]] {
      def F: Traverse[F]

      def R: Traverse[R]

      override def traverse[G[_], A, B](fa: F[R[A]])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[F[R[B]]] = {
        F.traverse(fa)(ra => R.traverse(ra)(f))
      }

      override def foldLeft[A, B](fa: F[R[A]], b: B)(f: (B, A) => B): B = F.foldLeft(fa, b)((b, ra) => R.foldLeft(ra, b)(f))

      override def foldRight[A, B](fa: F[R[A]], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
    }

    def composeTraverse[F[_], G[_]](implicit F1: Traverse[F], G1: Traverse[G]): Traverse[λ[α => F[G[α]]]] = new TraverseCompose[F, G] {
      override def F: Traverse[F] = F1

      override def R: Traverse[G] = G1
    }

    val traverseListOption = composeTraverse[List, Option]

    traverseListOption.sequence(List(Some(validateOk10), Some(validateError1), Some(validateError23))) shouldBe Validation.Failure(NonEmptyList("Error1", "Error2" :: "Error3" :: Nil))
    traverseListOption.sequence(List(Some(validateOk10))) shouldBe Validation.Success(List(Some(10)))
    traverseListOption.sequence(List.empty[Option[Validation[String, Int]]]) shouldBe Validation.Success(Nil)
    traverseListOption.sequence(List(Option.empty[Validation[String, Int]])) shouldBe Validation.Success(List(None))


    trait ComposeMonad[M[_], G[_]] extends Monad[λ[α => M[G[α]]]] {
      def M: Monad[M]

      def G: Monad[G]

      def T: Traverse[G]

      override def pure[A](value: => A): M[G[A]] = M.pure(G.pure(value))

      override def flatMap[A, B](ma: M[G[A]])(f: A => M[G[B]]): M[G[B]] = M.flatMap(ma) { ga =>
        M.map(T.traverse(ga)(f)(Monad.toApplicative(M)))(ggb => G.flatten(ggb))
      }
    }

    def composeMonad[M[_], G[_]](implicit M1: Monad[M], G1: Monad[G], T1: Traverse[G]): Monad[λ[α => M[G[α]]]] = new ComposeMonad[M, G] {
      override def M: Monad[M] = M1

      override def G: Monad[G] = G1

      override def T: Traverse[G] = T1
    }

    val listOptionMonad = composeMonad[List, Option]

    listOptionMonad.pure(10) shouldBe List(Some(10))
    listOptionMonad.flatten(List(Some(List(Some(10), None)), Some(List(Some(20), None)), None, Some(Nil))) shouldBe List(Some(10), None, Some(20), None, None)


  }
}
