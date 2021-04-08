package data1

import monoid.Semigroup


case class Kleisli[C, -A, +B](run: A => (B, C)) {
  def map[D](f: B => D): Kleisli[C, A, D] = Kleisli { a: A =>
    val (left, right) = run(a)
    f(left) -> right
  }

  def flatMap[D](f: B => Kleisli[C, B, D])(implicit semigroup: Semigroup[C]): Kleisli[C, A, D] =
    Kleisli { a: A =>
      val (left1, right1) = run(a)
      val (left2, right2) = f(left1).run(left1)
      val right = semigroup.combine(right1, right2)
      left2 -> right
    }
}

object Kleisli {
  def empty[A, B](run: A => B): Kleisli[String, A, B] = Kleisli { x: A =>
    run(x) -> ""
  }

}
