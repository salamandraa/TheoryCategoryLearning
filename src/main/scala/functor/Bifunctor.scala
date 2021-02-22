package functor

trait Bifunctor[F[_, _]] {

  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]
}

object Bifunctor extends BifunctorInstance {
  def apply[F[_, _]](implicit bifunctor: Bifunctor[F]): Bifunctor[F] = bifunctor
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
