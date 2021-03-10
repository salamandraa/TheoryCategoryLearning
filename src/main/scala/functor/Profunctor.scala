package functor

trait Profunctor[P[_, _]] {
  def dimap[A, B, C, D](pbc: P[B, C])(f: A => B)(g: C => D): P[A, D]

  def lmap[A, B, C](pbc: P[B, C])(f: A => B): P[A, C] = dimap(pbc)(f)(identity)

  def rmap[B, C, D](pbc: P[B, C])(g: C => D): P[B, D] = dimap(pbc)(identity[B])(g)
}

object Profunctor extends ProfunctorInstance {
  def apply[P[_, _]](implicit profunctor: Profunctor[P]): Profunctor[P] = profunctor
}

trait ProfunctorInstance {
  implicit val profunctorFunction: Profunctor[Function] = new Profunctor[Function] {
    override def dimap[A, B, C, D](pbc: B => C)(f: A => B)(g: C => D): A => D = g.compose(pbc).compose(f)
  }
}

trait ProfunctorLaws {

}