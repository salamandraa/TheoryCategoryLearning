package monada

import scala.util.{Failure, Success, Try}

trait Monad[M[_]] {
  def pure[A](value: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))

  def map2[A, B, C](fa: M[A], fb: M[B])(f: (A, B) => C): M[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))

  def map3[A, B, C, D](fa: M[A], fb: M[B], fc: M[C])(f: (A, B, C) => D): M[D] = flatMap(fa)(a => map2(fb, fc)((b, c) => f(a, b, c)))

  def map4[A, B, C, D, E](fa: M[A], fb: M[B], fc: M[C], fd: M[D])(f: (A, B, C, D) => E): M[E] = flatMap(fa)(a => map3(fb, fc, fd)((b, c, d) => f(a, b, c, d)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = {
    map(lma.foldLeft(pure(List.empty[A]))((mListA, mA) => flatMap(mA)(a => map(mListA)(listA => a :: listA))))(_.reverse)
    //    lma match {
    //    case headM :: tailM => flatMap(sequence(tailM))(tailA => map(headM)(headA => headA :: tailA))
    //    //    case headM :: tailM => flatMap(headM)(headA => map(sequence(tailM))(tailA => headA :: tailA))
    //    case Nil => pure(Nil)
    //  }
  }


  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = {
    map(la.foldLeft(pure(List.empty[B]))((mListB, a) => flatMap(f(a))(b => map(mListB)(listB => b :: listB))))(_.reverse)
    //    la match {
    //      case headA :: tailA => flatMap(traverse(tailA)(f))(tailB => map(f(headA))(headB => headB :: tailB))
    //      //    case headA :: tailA => flatMap(f(headA))(headB => map(traverse(tailA)(f))(tailB => headB :: tailB))
    //      case Nil => pure(Nil)
    //    }
  }

  //  EXERCISE 11.4
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = map(ma)(a => List.fill(n)(a))

  //  EXERCISE 11.5
  def product[A, B](ma: M[A], mb: M[B]): M[(A, B)] = flatMap(ma)(a => map(mb)(b => a -> b))

  //  EXERCISE 11.6
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {
    val fun: A => M[(A, Boolean)] = (x: A) => map(f(x))(boolean => x -> boolean)
    map(traverse(ms)(fun))(listPair => listPair.filter(_._2).map(_._1))
  }

}

object Monad extends MonadInstance {
  def apply[M[_]](implicit e: Monad[M]): Monad[M] = e
}

trait MonadInstance {

  implicit val monadaList: Monad[List] = new Monad[List] {
    override def pure[A](value: => A): List[A] = List(value)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  implicit val monadaSeq: Monad[Seq] = new Monad[Seq] {
    override def pure[A](value: => A): Seq[A] = Seq(value)

    override def flatMap[A, B](ma: Seq[A])(f: A => Seq[B]): Seq[B] = ma.flatMap(f)
  }

  implicit val monadaOpt: Monad[Option] = new Monad[Option] {
    override def pure[A](value: => A): Option[A] = Option(value)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma match {
      case Some(a) => f(a)
      case None => None
    }
  }

  implicit val monadaTry: Monad[Try] = new Monad[Try] {
    override def pure[A](value: => A): Try[A] = Try(value)

    override def flatMap[A, B](ma: Try[A])(f: A => Try[B]): Try[B] = ma match {
      case Failure(exception) => Failure(exception)
      case Success(a) => f(a)
    }
  }


}