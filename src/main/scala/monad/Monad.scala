package monad

import cats.Applicative
import data.Id.Id
import data.Reader
import monad.Monad.ComposeMonad

import scala.util.{Failure, Success, Try}

trait Monad[M[_]] {
  self =>
  def pure[A](value: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))

  def flatten[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

  def map2[A, B, C](fa: M[A], fb: M[B])(f: (A, B) => C): M[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))

  def map3[A, B, C, D](fa: M[A], fb: M[B], fc: M[C])(f: (A, B, C) => D): M[D] = flatMap(fa)(a => map2(fb, fc)((b, c) => f(a, b, c)))

  def map4[A, B, C, D, E](fa: M[A], fb: M[B], fc: M[C], fd: M[D])(f: (A, B, C, D) => E): M[E] = flatMap(fa)(a => map3(fb, fc, fd)((b, c, d) => f(a, b, c, d)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = {
    map(lma.foldLeft(pure(List.empty[A]))((mListA, mA) => flatMap(mA)(a => map(mListA)(listA => a :: listA))))(_.reverse)
    //    lma match {
    //    case headM :: tailM => flatMap(sequence(tailM))(tailA => map(headM)(headA => headA :: tailA))
    //    //    case headM :: tailM => flatMap(headM)(headA => map(sequence(tailM))(tailA => headA :: tailA))
    //    case Nil => pure(Nil)
    //  }
  }


  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = {
    map(la.foldLeft(pure(List.empty[B]))((mListB, a) => flatMap(f(a))(b => map(mListB)(listB => b :: listB))))(_.reverse)
    //    la match {
    //      case headA :: tailA => flatMap(traverse(tailA)(f))(tailB => map(f(headA))(headB => headB :: tailB))
    //      //    case headA :: tailA => flatMap(f(headA))(headB => map(traverse(tailA)(f))(tailB => headB :: tailB))
    //      case Nil => pure(Nil)
    //    }
  }

  //  EXERCISE 11.4
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = map(ma)(a => List.fill(n)(a))

  //  EXERCISE 11.5
  def product[A, B](ma: M[A], mb: M[B]): M[(A, B)] = flatMap(ma)(a => map(mb)(b => a -> b))

  //  EXERCISE 11.6
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {
    val fun: A => M[(A, Boolean)] = (x: A) => map(f(x))(boolean => x -> boolean)
    map(traverse(ms)(fun))(listPair => listPair.filter(_._2).map(_._1))
  }

  //  EXERCISE 12.11
  //  Try to write compose on Monad . It’s not possible, but it is instructive to attempt it and
  //    understand why this is the case.
  def compose[G[_]](implicit monadG: Monad[G]): Monad[λ[α => M[G[α]]]] = new ComposeMonad[M, G] {
    override def M: Monad[M] = self

    override def G: Monad[G] = monadG
  }

}

object Monad extends MonadInstance {

  trait ComposeMonad[M[_], G[_]] extends Monad[λ[α => M[G[α]]]] {
    def M: Monad[M]

    def G: Monad[G]

    override def pure[A](value: => A): M[G[A]] = M.pure(G.pure(value))

    override def flatMap[A, B](ma: M[G[A]])(f: A => M[G[B]]): M[G[B]] = ??? //M.flatMap(ma)(ga => G.flatMap(ga)(a => f(a)) )
  }

  def apply[M[_]](implicit e: Monad[M]): Monad[M] = e

  //  EXERCISE 12.1
  def toApplicative[M[_] : Monad]: Applicative[M] = new Applicative[M] {
    override def pure[A](x: A): M[A] = Monad[M].pure(x)

    override def ap[A, B](ff: M[A => B])(fa: M[A]): M[B] = {
      val monad = Monad[M]
      monad.flatMap(fa)(a => monad.map(ff)(f => f(a)))
    }
  }
}

trait MonadInstance {

  implicit val monadaList: Monad[List] = new Monad[List] {
    override def pure[A](value: => A): List[A] = List(value)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  implicit val monadaSeq: Monad[Seq] = new Monad[Seq] {
    override def pure[A](value: => A): Seq[A] = Seq(value)

    override def flatMap[A, B](ma: Seq[A])(f: A => Seq[B]): Seq[B] = ma.flatMap(f)
  }

  implicit val monadaOpt: Monad[Option] = new Monad[Option] {
    override def pure[A](value: => A): Option[A] = Option(value)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma match {
      case Some(a) => f(a)
      case None => None
    }
  }

  implicit val monadaTry: Monad[Try] = new Monad[Try] {
    override def pure[A](value: => A): Try[A] = Try(value)

    override def flatMap[A, B](ma: Try[A])(f: A => Try[B]): Try[B] = ma match {
      case Failure(exception) => Failure(exception)
      case Success(a) => f(a)
    }
  }

  //  EXERCISE 11.17
  implicit val monadId: Monad[Id] = new Monad[Id] {
    override def pure[A](value: => A): Id[A] = value

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma)
  }

  //  EXERCISE 11.20
  implicit def monadReader[T]: Monad[Reader[T, *]] = new Monad[Reader[T, *]] {
    override def pure[A](value: => A): Reader[T, A] = Reader(_ => value)

    override def flatMap[A, B](ma: Reader[T, A])(f: A => Reader[T, B]): Reader[T, B] = Reader {
      t: T => f.compose(ma.fun).apply(t).fun(t)
    }
  }

  implicit def monadEither[L]: Monad[Either[L, *]] = new Monad[Either[L, *]] {

    override def pure[A](value: => A): Either[L, A] = Right(value)

    override def flatMap[A, B](ma: Either[L, A])(f: A => Either[L, B]): Either[L, B] = ma.flatMap(f)
  }

  //  EXERCISE 12.5
  def monadEither2[L]: Monad[Either[L, *]] = new Monad[({type f[x] = Either[L, x]})#f] {
    override def pure[A](value: => A): Either[L, A] = Right(value)

    override def flatMap[A, B](ma: Either[L, A])(f: A => Either[L, B]): Either[L, B] = ma.flatMap(f)
  }


}