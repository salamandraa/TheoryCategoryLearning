package data

/**
 * !!! NOT work !!!
 */
sealed trait PreList[+T, +F[_]]

case object PreNil extends PreList[Nothing, Nothing]

case class Cons[T, F[_]](head: T, tail: F[T]) extends PreList[T, F]


