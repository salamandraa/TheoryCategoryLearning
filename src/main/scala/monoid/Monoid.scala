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

  implicit class MonoidIndexedSeqFun[A](v: IndexedSeq[A]) {
    //    EXERCISE 10.7
    def foldMapV[B](f: A => B)(implicit m: Monoid[B]): B = {
      if (v.size <= 1) v.headOption.map(f).getOrElse(m.empty)
      else {
        val (l, r) = v.splitAt(v.size / 2)
        m.combine(l.foldMapV(f), r.foldMapV(f))
      }
    }
  }

  //  EXERCISE 10.18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = Foldable[Seq].foldMap(as)(x => Map(x -> 1))

}

trait MonoidInstance {
  implicit val monoidIntSum: Monoid[Int] = new Monoid[Int] {

    override def combine(a: Int, b: Int): Int = Semigroup.semigroupInt.combine(a, b)

    override def empty: Int = 0

  }

  implicit val monoidString: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(a: String, b: String): String = Semigroup.semigroupString.combine(a, b)
  }


  implicit def monoidSet[T]: Monoid[Set[T]] = new Monoid[Set[T]] {
    override def empty: Set[T] = Set.empty

    override def combine(a: Set[T], b: Set[T]): Set[T] = a union b
  }

  implicit def monoidSeq[T]: Monoid[Seq[T]] = new Monoid[Seq[T]] {
    override def empty: Seq[T] = Seq.empty[T]

    override def combine(a: Seq[T], b: Seq[T]): Seq[T] = Semigroup.semigroupSeq.combine(a, b)
  }


  implicit def monoidOpt[T: Semigroup]: Monoid[Option[T]] = new Monoid[Option[T]] {
    override def empty: Option[T] = None

    override def combine(aOpt: Option[T], bOpt: Option[T]): Option[T] = Semigroup.semigroupOpt(implicitly(Semigroup[T])).combine(aOpt, bOpt)

  }

  implicit def monoidTuple2[T1, T2](implicit e1: Monoid[T1], e2: Monoid[T2]): Monoid[(T1, T2)] = new Monoid[(T1, T2)] {
    override def empty: (T1, T2) = e1.empty -> e2.empty

    override def combine(a: (T1, T2), b: (T1, T2)): (T1, T2) = Semigroup.semigroupTuple2(e1, e2).combine(a, b)
  }

  implicit def monoidMap[T1, T2](implicit e2: Semigroup[T2]): Monoid[Map[T1, T2]] = new Monoid[Map[T1, T2]] {
    override def empty: Map[T1, T2] = Map.empty

    override def combine(a: Map[T1, T2], b: Map[T1, T2]): Map[T1, T2] = Semigroup.semigroupMap(e2).combine(a, b)
  }

  //exercise 10.1
  val monoidIntMultiplication: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 1

    override def combine(a: Int, b: Int): Int = a * b
  }

  val monoidOrBoolean: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(a: Boolean, b: Boolean): Boolean = a || b
  }

  val monoidAndBoolean: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(a: Boolean, b: Boolean): Boolean = a && b
  }
  //exercise 10.3

  implicit def monoidEndoFunction[T]: Monoid[T => T] = new Monoid[T => T] {
    override def empty: T => T = x => x

    override def combine(a: T => T, b: T => T): T => T = a.andThen(b)
  }


  implicit def monoidList[T]: Monoid[List[T]] = new Monoid[List[T]] {
    override def empty: List[T] = Nil

    override def combine(a: List[T], b: List[T]): List[T] = Semigroup.semigroupList[T].combine(a, b)
  }

  //  EXERCISE 10.17
  implicit def functionMonoid[A, B](implicit mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def empty: A => B = _ => mb.empty

    override def combine(a: A => B, b: A => B): A => B = x => mb.combine(a(x), b(x))
  }

  implicit val monoidDouble: Monoid[Double] = new Monoid[Double] {
    override def empty: Double = 0.0

    override def combine(a: Double, b: Double): Double = a + b
  }

  case class Order(totalCost: Double, quantity: Double)

  implicit val monoidOrder: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(Monoid[Double].empty, Monoid[Double].empty)

    override def combine(a: Order, b: Order): Order = Order(Monoid[Double].combine(a.totalCost, b.totalCost), Monoid[Double].combine(a.quantity, b.quantity))
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