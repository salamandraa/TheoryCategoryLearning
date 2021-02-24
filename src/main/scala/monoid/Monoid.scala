package monoid

trait Monoid[T] extends Semigroup[T] {
  def empty: T

  def combineAll(seq: Seq[T]): T = seq.foldLeft(this.empty)(this.combine)
}

object Monoid extends MonoidInstance {

  implicit class MonoidListFun[T](a: Seq[T]) {
    def foldMap[R](f: T => R)(implicit im: Monoid[R]): R = a.foldLeft(im.empty)((res, value) => im.combine(res, f(value)))
  }

  def apply[T](implicit e: Monoid[T]): Monoid[T] = e

  def combineAll[T](seq: Seq[T])(implicit im: Monoid[T]): T = seq.foldLeft(im.empty)(im.combine)

}

trait MonoidInstance {
  implicit val monoidInt: Monoid[Int] = new Monoid[Int] {

    override def combine(a: Int, b: Int): Int = Semigroup.semigroupInt.combine(a, b)

    override def empty: Int = 0

  }

  implicit val monoidString: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(a: String, b: String): String = Semigroup.semigroupString.combine(a, b)
  }

  implicit def monoidSeq[T]: Monoid[Seq[T]] = new Monoid[Seq[T]] {
    override def empty: Seq[T] = Seq.empty[T]

    override def combine(a: Seq[T], b: Seq[T]): Seq[T] = Semigroup.semigroupSeq.combine(a, b)
  }


  implicit def monoidOpt[T: Monoid]: Monoid[Option[T]] = new Monoid[Option[T]] {
    override def empty: Option[T] = None

    override def combine(aOpt: Option[T], bOpt: Option[T]): Option[T] = Semigroup.semigroupOpt(implicitly(Semigroup[T])).combine(aOpt, bOpt)

  }

  implicit def monoidTuple2[T1, T2](implicit e1: Monoid[T1], e2: Monoid[T2]): Monoid[(T1, T2)] = new Monoid[(T1, T2)] {
    override def empty: (T1, T2) = e1.empty -> e2.empty

    override def combine(a: (T1, T2), b: (T1, T2)): (T1, T2) = Semigroup.semigroupTuple2(e1, e2).combine(a, b)
  }

  implicit def monoidMap[T1, T2](implicit e2: Monoid[T2]): Monoid[Map[T1, T2]] = new Monoid[Map[T1, T2]] {
    override def empty: Map[T1, T2] = Map.empty

    override def combine(a: Map[T1, T2], b: Map[T1, T2]): Map[T1, T2] = Semigroup.semigroupMap(e2).combine(a, b)
  }

}

trait MonoidLaws {
  def identityLaw[A: Monoid](a: A): Boolean = {
    val left = identityLawLeft(a)
    val right = identityLawRight(a)
    left == right && left == a
  }

  def identityLawLeft[A: Monoid](a: A): A = {
    val monoid = implicitly[Monoid[A]]
    monoid.combine(monoid.empty, a)
  }

  def identityLawRight[A: Monoid](a: A): A = {
    val monoid = implicitly[Monoid[A]]
    monoid.combine(a, monoid.empty)
  }

  def compositionLaw[A: Monoid](a: A, b: A, c: A): Boolean = compositionLawLeft(a, b, c) == compositionLawRight(a, b, c)

  def compositionLawLeft[A: Monoid](a: A, b: A, c: A): A = {
    val monoid = implicitly[Monoid[A]]
    monoid.combine(monoid.combine(a, b), c)
  }

  def compositionLawRight[A: Monoid](a: A, b: A, c: A): A = {
    val monoid = implicitly[Monoid[A]]
    monoid.combine(a, monoid.combine(b, c))
  }
}