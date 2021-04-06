package data

import cats.Applicative
import cats.data.NonEmptyList

sealed trait Validation[+E, +A]

object Validation {

  case class Failure[E](errors: NonEmptyList[E])
    extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  //  EXERCISE 12.6
  implicit def applicativeValidation[E]: Applicative[Validation[E, *]] = new Applicative[Validation[E, *]] {
    override def pure[A](x: A): Validation[E, A] = Success(x)

    override def ap[A, B](ff: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] = {
      ff match {
        case Failure(errors1) => fa match {
          case Failure(errors2) => Failure(NonEmptyList.fromList(errors1.toList ++ errors2.toList).get)
          case _ => Failure(errors1)
        }
        case Success(f) => fa match {
          case Failure(errors) => Failure(errors)
          case Success(a) => Success(f(a))
        }
      }
    }
  }
}

