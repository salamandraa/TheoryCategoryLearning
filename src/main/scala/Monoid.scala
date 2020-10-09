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

  implicit def monoidOpt[T: Monoid]: Monoid[Option[T]] = new Monoid[Option[T]] {
    override def empty: Option[T] = None

    override def combine(aOpt: Option[T], bOpt: Option[T]): Option[T] = {
      aOpt -> bOpt match {
        case (Some(a), Some(b)) => Some(implicitly(Monoid[T]).combine(a, b))
        case (Some(_), None) => aOpt
        case _ => bOpt
      }
    }
  }

}
