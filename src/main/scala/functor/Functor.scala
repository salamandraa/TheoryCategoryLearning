package functor

import data.{Branch, Const, Leaf, Reader, Tree}
import data.Id.Id

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait Functor[F[_]] {
  self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = map(fab)(_._1) -> map(fab)(_._2)

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(value) => map(value)(Left(_))
    case Right(value) => map(value)(Right(_))
  }

  def lift[A, B](f: A => B): F[A] => F[B] = (fa: F[A]) => map(fa)(f)

  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => a -> f(a))


  def compose[G[_]](implicit functorG: Functor[G]): Functor[λ[α => F[G[α]]]] = new Functor.ComposeCovariantFunctor[F, G] {
    override def F: Functor[F] = self

    override def G: Functor[G] = functorG
  }
}

object Functor extends FunctorInstance {

  // TODO:  use data.state !!!!
  trait ComposeCovariantFunctor[F[_], G[_]] extends Functor[λ[α => F[G[α]]]] {
    def F: Functor[F]

    def G: Functor[G]

    override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = {
      F.map(fga)(ga => G.map(ga)(f))
    }
  }

  def apply[F[_]](implicit e: Functor[F]): Functor[F] = e
}


trait FunctorInstance {
  // TODO:  use data.state !!!!

  implicit def functorConst[C]: Functor[Const[C, *]] = new Functor[Const[C, *]] {
    override def map[A, B](fa: Const[C, A])(f: A => B): Const[C, B] = Const(fa.value)
  }

  implicit def functorReader[T]: Functor[Reader[T, *]] = new Functor[Reader[T, *]] {
    override def map[A, B](fa: Reader[T, A])(f: A => B): Reader[T, B] = Reader(f.compose(fa.fun))
  }

  implicit def functorReaderAsFunction[T]: Functor[Function[T, *]] = new Functor[T => *] {
    override def map[A, B](fa: T => A)(f: A => B): T => B = f.compose(fa)
  }

  implicit val functorId: Functor[Id] = new Functor[Id] {
    override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
  }

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

  implicit def functorFuture(implicit ec: ExecutionContext): Functor[Future] = new Functor[Future] {
    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }

  implicit val functorTree: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }
}

trait FunctorLaws {
  def identityLaw[F[_] : Functor, A](fa: F[A]): Boolean = {
    val left = identityLawLeft(fa)
    val right = identityLawRight(fa)
    left == right && left == fa
  }

  def identityLawLeft[F[_] : Functor, A](fa: F[A]): F[A] = identity(fa)

  def identityLawRight[F[_] : Functor, A](fa: F[A]): F[A] = implicitly[Functor[F]].map(fa)(identity)

  def compositionLaw[F[_] : Functor, A, B, C](fa: F[A])(f: A => B)(g: B => C): Boolean = compositionLawLeft(fa)(f)(g) == compositionLawRight(fa)(f)(g)

  def compositionLawLeft[F[_] : Functor, A, B, C](fa: F[A])(f: A => B)(g: B => C): F[C] = {
    val functor = implicitly[Functor[F]]
    functor.map(functor.map(fa)(f))(g)
  }

  def compositionLawRight[F[_] : Functor, A, B, C](fa: F[A])(f: A => B)(g: B => C): F[C] = {
    val functor = implicitly[Functor[F]]
    functor.map(fa)(f.andThen(g))
  }
}