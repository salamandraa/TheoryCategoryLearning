package functor

import data.Op.Op
import data.{Box, Printable}

trait ContravariantFunctor[C[_]] {
  def contramap[A, B](c: C[A])(f: B => A): C[B]
}

object ContravariantFunctor extends ContravariantInstance {
  def apply[C[_]](implicit contravariant: ContravariantFunctor[C]): ContravariantFunctor[C] = contravariant
}

trait ContravariantInstance {
  implicit def contravariantOp[R]: ContravariantFunctor[Op[R, *]] = new ContravariantFunctor[Op[R, *]] {
    override def contramap[A, B](c: Op[R, A])(f: B => A): Op[R, B] = c.compose(f)
  }


  implicit val contravariantFunctorOrdering: ContravariantFunctor[Ordering] = new ContravariantFunctor[Ordering[*]] {
    override def contramap[A, B](c: Ordering[A])(f: B => A): Ordering[B] = c.on(f)
  }

  implicit val contravariantFunctorEquiv: ContravariantFunctor[Equiv] = new ContravariantFunctor[Equiv[*]] {
    override def contramap[A, B](c: Equiv[A])(f: B => A): Equiv[B] = Equiv.fromFunction[B] { case (l, r) => c.equiv(f(l), f(r)) }
  }

  implicit val contravariantFunctorPrintable: ContravariantFunctor[Printable] = new ContravariantFunctor[Printable] {
    override def contramap[A, B](c: Printable[A])(f: B => A): Printable[B] = (value: B) => c.format(f(value))
  }
}

trait ContravariantFunctorLaws {
  def identityLaw[F[_] : ContravariantFunctor, A](fa: F[A]): Boolean = {
    val left = identityLawLeft(fa)
    val right = identityLawRight(fa)
    left == right && left == fa
  }

  def identityLawLeft[F[_] : ContravariantFunctor, A](fa: F[A]): F[A] = identity(fa)

  def identityLawRight[F[_] : ContravariantFunctor, A](fa: F[A]): F[A] = implicitly[ContravariantFunctor[F]].contramap(fa)(identity)

  def compositionLaw[F[_] : ContravariantFunctor, A, B, C](fa: F[A])(f: B => A)(g: C => B): Boolean = compositionLawLeft(fa)(f)(g) == compositionLawRight(fa)(f)(g)

  def compositionLawLeft[F[_] : ContravariantFunctor, A, B, C](fa: F[A])(f: B => A)(g: C => B): F[C] = {
    val functor = implicitly[ContravariantFunctor[F]]
    functor.contramap(functor.contramap(fa)(f))(g)
  }

  def compositionLawRight[F[_] : ContravariantFunctor, A, B, C](fa: F[A])(f: B => A)(g: C => B): F[C] = {
    val functor = implicitly[ContravariantFunctor[F]]
    functor.contramap(fa)(f.compose(g))
  }
}



