package functor

import scala.util.{Failure, Success, Try}

trait Functor[F[_]] {
  self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] = (fa: F[A]) => map(fa)(f)

  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => a -> f(a))


  def compose[G[_]](implicit functorG: Functor[G]): Functor[λ[α => F[G[α]]]] = new Functor.ComposeFunctor[F, G] {
    override def F: Functor[F] = self

    override def G: Functor[G] = functorG
  }
}

object Functor extends FunctorInstance {

  trait ComposeFunctor[F[_], G[_]] extends Functor[λ[α => F[G[α]]]] {
    def F: Functor[F]

    def G: Functor[G]

    override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = {
      F.map(fga)(ga => G.map(ga)(f))
    }
  }

  def apply[F[_]](implicit e: Functor[F]): Functor[F] = e
}


trait FunctorInstance {

  /**
   * example infer Functor[List]
   */
  implicit def toFunctorList(implicit fun: Functor[Seq]): Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fun.map(fa)(f).toList
  }

  implicit val functorSeq: Functor[Seq] = new Functor[Seq] {
    override def map[A, B](fa: Seq[A])(f: A => B): Seq[B] = fa.map(f)
  }

  implicit val functorOpt: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case Some(value) => Some(f(value))
      case None => None
    }
  }

  implicit val functorSet: Functor[Set] = new Functor[Set] {
    override def map[A, B](fa: Set[A])(f: A => B): Set[B] = fa.map(f)
  }

  implicit val functorTry: Functor[Try] = new Functor[Try] {
    override def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa match {
      case Failure(exception) => Failure(exception)
      case Success(value) => Success(f(value))
    }
  }
}