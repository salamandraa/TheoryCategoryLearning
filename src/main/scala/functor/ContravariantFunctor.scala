package functor

import data.Op.Op

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


  implicit val functorOrdering: ContravariantFunctor[Ordering] = new ContravariantFunctor[Ordering] {
    override def contramap[A, B](c: Ordering[A])(f: B => A): Ordering[B] = c.on(f)
  }

  implicit val functorEquiv: ContravariantFunctor[Equiv] = new ContravariantFunctor[Equiv] {
    override def contramap[A, B](c: Equiv[A])(f: B => A): Equiv[B] = Equiv.fromFunction[B] { case (l, r) => c.equiv(f(l), f(r)) }
  }
}



