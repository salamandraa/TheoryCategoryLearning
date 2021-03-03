package monada

import scala.util.{Failure, Success, Try}

trait Monad[M[_]] {
  def pure[A](value: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))

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