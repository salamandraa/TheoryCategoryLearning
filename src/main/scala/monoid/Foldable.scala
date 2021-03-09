package monoid

import data.{Branch, Leaf, Tree}

import scala.annotation.tailrec

trait Foldable[F[_]] {
  def foldRight[A, B](fa: F[A], zero: B)(f: (A, B) => B): B

  def foldLeft[A, B](fa: F[A], zero: B)(f: (B, A) => B): B

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit mb: Monoid[B]): B

  def fold[A](fa: F[A])(implicit ma: Monoid[A]): A = foldLeft(fa, ma.empty)(ma.combine)

  //EXERCISE 10.15
  def toList[A](fa: F[A]): List[A]

  def find[A](fa: F[A])(f: A => Boolean): Option[A] = foldLeft(fa, Option.empty[A])((b, a) => b.orElse(Some(a).filter(f)))

  def exists[A](fa: F[A])(f: A => Boolean): Boolean = foldLeft(fa, false)((b, a) => b || f(a))

  def forall[A](fa: F[A])(f: A => Boolean): Boolean = foldLeft(fa, true)((b, a) => b && f(a))

  def filter_[A](fa: F[A])(f: A => Boolean): List[A] = foldRight(fa, List.empty[A])((a, b) => if (f(a)) a :: b else b)

  def isEmpty[A](fa: F[A]): Boolean = foldLeft(fa, true)((_, _) => false)
}

object Foldable extends FoldableInstance {
  def apply[F[_]](implicit foldable: Foldable[F]): Foldable[F] = foldable
}

trait FoldableInstance {
  //  EXERCISE 10.12
  implicit val foldableList: Foldable[List] = new Foldable[List] {
    override def foldRight[A, B](fa: List[A], zero: B)(f: (A, B) => B): B = foldLeft(fa.reverse, zero)((b, a) => f(a, b))

    @tailrec
    override def foldLeft[A, B](fa: List[A], zero: B)(f: (B, A) => B): B = fa match {
      case head :: tail => foldLeft(tail, f(zero, head))(f)
      case Nil => zero
    }

    override def foldMap[A, B](fa: List[A])(f: A => B)(implicit mb: Monoid[B]): B = fa.foldLeft(mb.empty) { case (l, r) => mb.combine(l, f(r)) }

    override def toList[A](fa: List[A]): List[A] = fa
  }

  implicit val foldableSeq: Foldable[Seq] = new Foldable[Seq] {
    override def foldRight[A, B](fa: Seq[A], zero: B)(f: (A, B) => B): B = fa.foldRight(zero)(f)

    override def foldLeft[A, B](fa: Seq[A], zero: B)(f: (B, A) => B): B = fa.foldLeft(zero)(f)

    override def foldMap[A, B](fa: Seq[A])(f: A => B)(implicit mb: Monoid[B]): B = fa.map(f).fold(mb.empty)(mb.combine)

    override def toList[A](fa: Seq[A]): List[A] = fa.toList
  }

  //    EXERCISE 10.13
  implicit val foldableTree: Foldable[Tree] = new Foldable[Tree] {
    override def foldRight[A, B](fa: Tree[A], zero: B)(f: (A, B) => B): B = fa match {
      case Leaf(value) => f(value, zero)
      case Branch(left, right) => foldRight(left, foldRight(right, zero)(f))(f)
    }

    override def foldLeft[A, B](fa: Tree[A], zero: B)(f: (B, A) => B): B = fa match {
      case Leaf(value) => f(zero, value)
      case Branch(left, right) => foldLeft(right, foldLeft(left, zero)(f))(f)
    }

    override def foldMap[A, B](fa: Tree[A])(f: A => B)(implicit mb: Monoid[B]): B = fa match {
      case Leaf(value) => f(value)
      case Branch(left, right) => mb.combine(foldMap(left)(f), foldMap(right)(f))
    }

    override def toList[A](fa: Tree[A]): List[A] = fa match {
      case Leaf(value) => List(value)
      case Branch(left, right) => Monoid.monoidList.combine(toList(left), toList(right))
    }
  }

  //  EXERCISE 10.14
  implicit val foldableOption: Foldable[Option] = new Foldable[Option] {
    override def foldRight[A, B](fa: Option[A], zero: B)(f: (A, B) => B): B = fa match {
      case Some(value) => f(value, zero)
      case None => zero
    }

    override def foldLeft[A, B](fa: Option[A], zero: B)(f: (B, A) => B): B = fa match {
      case Some(value) => f(zero, value)
      case None => zero
    }

    override def foldMap[A, B](fa: Option[A])(f: A => B)(implicit mb: Monoid[B]): B = {
      fa match {
        case Some(value) => f(value)
        case None => mb.empty
      }
    }

    override def toList[A](fa: Option[A]): List[A] = fa match {
      case Some(value) => List(value)
      case None => Nil
    }
  }
}