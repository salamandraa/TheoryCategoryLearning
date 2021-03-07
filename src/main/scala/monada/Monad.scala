package monada

import scala.util.{Failure, Success, Try}

trait Monad[M[_]] {
  def pure[A](value: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))

  def map2[A, B, C](fa: M[A], fb: M[B])(f: (A, B) => C): M[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))

  def map3[A, B, C, D](fa: M[A], fb: M[B], fc: M[C])(f: (A, B, C) => D): M[D] = flatMap(fa)(a => map2(fb, fc)((b, c) => f(a, b, c)))

  def map4[A, B, C, D, E](fa: M[A], fb: M[B], fc: M[C], fd: M[D])(f: (A, B, C, D) => E): M[E] = flatMap(fa)(a => map3(fb, fc, fd)((b, c, d) => f(a, b, c, d)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = lma match {
    case head :: tail => flatMap(head)(headA => map(sequence(tail))(tailA => headA :: tailA))
    case Nil => pure(Nil)
  }

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = la match {
    case head :: tail => flatMap(f(head))(headB => map(traverse(tail)(f))(tailB => headB :: tailB))
    case Nil => pure(Nil)
  }
}

object Monad extends MonadInstance {
  def apply[M[_]](implicit e: Monad[M]): Monad[M] = e
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


}