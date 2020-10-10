package monoid

trait Monoid[T] {
  def empty: T

  def combine(a: T, b: T): T
}

object Monoid {
  def apply[T](implicit e: Monoid[T]): Monoid[T] = e

  implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0

    override def combine(a: Int, b: Int): Int = a + b
  }

  implicit val monoidString: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(a: String, b: String): String = a + b
  }

  implicit def monoidSeq[T: Monoid]: Monoid[Seq[T]] = new Monoid[Seq[T]] {
    override def empty: Seq[T] = Seq.empty[T]

    override def combine(a: Seq[T], b: Seq[T]): Seq[T] = a ++ b
  }

  private def combineOpt[T: Monoid](a: T, bOpt: Option[T]): T = {
    bOpt match {
      case Some(b) => implicitly(Monoid[T]).combine(a, b)
      case None => a
    }
  }

  implicit def monoidOpt[T: Monoid]: Monoid[Option[T]] = new Monoid[Option[T]] {
    override def empty: Option[T] = None

    override def combine(aOpt: Option[T], bOpt: Option[T]): Option[T] = {
      aOpt -> bOpt match {
        case (Some(a), _) => Some(combineOpt(a, bOpt))
        case _ => bOpt
      }
    }
  }

  implicit def monoidTuple2[T1: Monoid, T2: Monoid]: Monoid[(T1, T2)] = new Monoid[(T1, T2)] {
    override def empty: (T1, T2) = implicitly(Monoid[T1]).empty -> implicitly(Monoid[T2]).empty

    override def combine(a: (T1, T2), b: (T1, T2)): (T1, T2) = implicitly(Monoid[T1]).combine(a._1, b._1) -> implicitly(Monoid[T2]).combine(a._2, b._2)
  }

}
