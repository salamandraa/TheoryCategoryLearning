package functor

trait Bifunctor[F[_, _]] {
  self =>

  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  def leftFunctor[X]: Functor[F[*, X]] = new Functor[λ[a => F[a, X]]] {
    override def map[A, B](fa: F[A, X])(f: A => B): F[B, X] = bimap(fa)(f, identity)
  }

  def rightFunctor[X]: Functor[λ[b => F[X, b]]] = new Functor[F[X, *]] {
    override def map[A, B](fa: F[X, A])(f: A => B): F[X, B] = bimap(fa)(identity, f)
  }

  def compose[G[_, _]](implicit bifunctorG: Bifunctor[G]): Bifunctor[λ[(a, b) => F[G[a, b], G[a, b]]]] = new Bifunctor.ComposeBifunctor[F, G] {
    override def F: Bifunctor[F] = self

    override def G: Bifunctor[G] = bifunctorG
  }
}

object Bifunctor extends BifunctorInstance {

  trait ComposeBifunctor[F[_, _], G[_, _]] extends Bifunctor[λ[(a, b) => F[G[a, b], G[a, b]]]] {
    def F: Bifunctor[F]

    def G: Bifunctor[G]

    override def bimap[A, B, C, D](fab: F[G[A, B], G[A, B]])(f: A => C, g: B => D): F[G[C, D], G[C, D]] = {
      F.bimap(fab)(gab1 => G.bimap(gab1)(f, g), gab2 => G.bimap(gab2)(f, g))
    }
  }

  def apply[F[_, _]](implicit bifunctor: Bifunctor[F]): Bifunctor[F] = bifunctor

  implicit class PairExtended[F[_, _], A, B](fab: F[A, B]) {
    def bimap[C, D](f: A => C, g: B => D)(implicit bifunctor: Bifunctor[F]): F[C, D] = Bifunctor[F].bimap(fab)(f, g)
  }

}

trait BifunctorInstance {
  implicit val mapBifunctor: Bifunctor[Map] = new Bifunctor[Map] {
    override def bimap[A, B, C, D](fab: Map[A, B])(f: A => C, g: B => D): Map[C, D] = fab.map { case (a, b) => f(a) -> g(b) }
  }

  implicit val pairBifunctor: Bifunctor[Tuple2] = new Bifunctor[Tuple2] {
    override def bimap[A, B, C, D](fab: (A, B))(f: A => C, g: B => D): (C, D) = f(fab._1) -> g(fab._2)
  }
  implicit val eitherBifunctor: Bifunctor[Either] = new Bifunctor[Either] {
    override def bimap[A, B, C, D](fab: Either[A, B])(f: A => C, g: B => D): Either[C, D] = fab match {
      case Left(a) => Left(f(a))
      case Right(b) => Right(g(b))
    }
  }
}
