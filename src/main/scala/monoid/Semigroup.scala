package monoid

trait Semigroup[T] {
  def combine(a: T, b: T): T
}

object Semigroup {

  implicit class SemigroupFun[T: Semigroup](a: T) {
    def |+|(b: T): T = implicitly(Semigroup[T]).combine(a, b)
  }

  def apply[T](implicit e: Semigroup[T]): Semigroup[T] = e


  implicit val semigroupInt: Semigroup[Int] = (a: Int, b: Int) => a + b

  implicit val semigroupString: Semigroup[String] = (a: String, b: String) => a + b

  implicit def semigroupSeq[T]: Semigroup[Seq[T]] = (a: Seq[T], b: Seq[T]) => a ++ b

  implicit def semigroupList[T]: Semigroup[List[T]] = (a: List[T], b: List[T]) => a ++ b

  private def combineOpt[T: Semigroup](a: T, bOpt: Option[T]): T = {
    bOpt match {
      case Some(b) => implicitly(Semigroup[T]).combine(a, b)
      case None => a
    }
  }

  implicit def semigroupOpt[T: Semigroup]: Semigroup[Option[T]] = (aOpt: Option[T], bOpt: Option[T]) => {
    aOpt -> bOpt match {
      case (Some(a), _) => Some(combineOpt(a, bOpt))
      case _ => bOpt
    }
  }

  implicit def semigroupTuple2[T1: Semigroup, T2: Semigroup]: Semigroup[(T1, T2)] = (a: (T1, T2), b: (T1, T2)) => implicitly(Semigroup[T1]).combine(a._1, b._1) -> implicitly(Semigroup[T2]).combine(a._2, b._2)

  implicit def semigroupFun1[T: Semigroup]: Semigroup[T => T] = (a: T => T, b: T => T) => a.andThen(b)

  implicit def semigroupMap[T1, T2](implicit e2: Semigroup[T2]): Semigroup[Map[T1, T2]] = (a: Map[T1, T2], b: Map[T1, T2]) => {
    a.foldLeft(b) { case (resMap, (key, value)) => resMap.get(key) match {
      case Some(valueThat) => resMap.updated(key, e2.combine(value, valueThat))
      case None => resMap
    }
    }
  }
}