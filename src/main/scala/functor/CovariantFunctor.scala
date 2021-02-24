package functor

import data.Const
import data.Id.Id
import data.Reader

import scala.util.{Failure, Success, Try}

trait CovariantFunctor[F[_]] {
  self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] = (fa: F[A]) => map(fa)(f)

  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => a -> f(a))


  def compose[G[_]](implicit functorG: CovariantFunctor[G]): CovariantFunctor[λ[α => F[G[α]]]] = new CovariantFunctor.ComposeCovariantFunctor[F, G] {
    override def F: CovariantFunctor[F] = self

    override def G: CovariantFunctor[G] = functorG
  }
}

object CovariantFunctor extends FunctorInstance {

  // TODO:  use data.state !!!!
  trait ComposeCovariantFunctor[F[_], G[_]] extends CovariantFunctor[λ[α => F[G[α]]]] {
    def F: CovariantFunctor[F]

    def G: CovariantFunctor[G]

    override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = {
      F.map(fga)(ga => G.map(ga)(f))
    }
  }

  def apply[F[_]](implicit e: CovariantFunctor[F]): CovariantFunctor[F] = e
}


trait FunctorInstance {
  // TODO:  use data.state !!!!

  implicit def functorConst[C]: CovariantFunctor[Const[C, *]] = new CovariantFunctor[Const[C, *]] {
    override def map[A, B](fa: Const[C, A])(f: A => B): Const[C, B] = Const(fa.value)
  }

  implicit def functorReader[T]: CovariantFunctor[Reader[T, *]] = new CovariantFunctor[Reader[T, *]] {
    override def map[A, B](fa: Reader[T, A])(f: A => B): Reader[T, B] = Reader(f.compose(fa.fun))
  }

  implicit def functorReaderAsFunction[T]: CovariantFunctor[Function[T, *]] = new CovariantFunctor[T => *] {
    override def map[A, B](fa: T => A)(f: A => B): T => B = f.compose(fa)
  }

  implicit val functorId: CovariantFunctor[Id] = new CovariantFunctor[Id] {
    override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
  }

  /**
   * example infer Functor[List]
   */
  implicit def toFunctorList(implicit fun: CovariantFunctor[Seq]): CovariantFunctor[List] = new CovariantFunctor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fun.map(fa)(f).toList
  }

  implicit val functorSeq: CovariantFunctor[Seq] = new CovariantFunctor[Seq] {
    override def map[A, B](fa: Seq[A])(f: A => B): Seq[B] = fa.map(f)
  }

  implicit val functorOpt: CovariantFunctor[Option] = new CovariantFunctor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case Some(value) => Some(f(value))
      case None => None
    }
  }

  implicit val functorSet: CovariantFunctor[Set] = new CovariantFunctor[Set] {
    override def map[A, B](fa: Set[A])(f: A => B): Set[B] = fa.map(f)
  }

  implicit val functorTry: CovariantFunctor[Try] = new CovariantFunctor[Try] {
    override def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa match {
      case Failure(exception) => Failure(exception)
      case Success(value) => Success(f(value))
    }
  }
}

trait FunctorLaw {
  def identityLaw[F[_] : CovariantFunctor, A](fa: F[A]): Boolean = identityLawLeft(fa) == identityLawRight(fa)

  def identityLawLeft[F[_] : CovariantFunctor, A](fa: F[A]): F[A] = identity(fa)

  def identityLawRight[F[_] : CovariantFunctor, A](fa: F[A]): F[A] = implicitly[CovariantFunctor[F]].map(fa)(identity)

  def compositionLaw[F[_] : CovariantFunctor, A, B, C](fa: F[A])(f: A => B)(g: B => C): Boolean = compositionLawLeft(fa)(f)(g) == compositionLawRight(fa)(f)(g)

  def compositionLawLeft[F[_] : CovariantFunctor, A, B, C](fa: F[A])(f: A => B)(g: B => C): F[C] = {
    val functor = implicitly[CovariantFunctor[F]]
    functor.map(functor.map(fa)(f))(g)
  }

  def compositionLawRight[F[_] : CovariantFunctor, A, B, C](fa: F[A])(f: A => B)(g: B => C): F[C] = {
    val functor = implicitly[CovariantFunctor[F]]
    functor.map(fa)(f.andThen(g))
  }
}