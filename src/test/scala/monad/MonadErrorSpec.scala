package monada

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Success, Try}

object MonadErrorSpec {
  implicit def monadErrorOption[E]: MonadError[Option, E] = new MonadError[Option, E] {
    override def pure[A](x: A): Option[A] = Some(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = ???

    override def raiseError[A](e: E): Option[A] = None

    override def handleErrorWith[A](fa: Option[A])(f: E => Option[A]): Option[A] = ???
  }
}

class MonadErrorSpec extends AnyFlatSpec with should.Matchers {

  import MonadErrorSpec.monadErrorOption

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] = {
    if (age >= 18) age.pure[F] else new IllegalArgumentException().raiseError[F, Int]
  }

  it should "test" in {
    validateAdult[Try](17).isFailure shouldBe true
    validateAdult[Try](18) shouldBe Success(18)

    validateAdult[Option](17) shouldBe None
    validateAdult[Option](18) shouldBe Some(18)

    type ErrorOrRight[R] = Either[Throwable, R]

    validateAdult[ErrorOrRight](17).isLeft shouldBe true
    validateAdult[ErrorOrRight](18) shouldBe Right(18)

  }
}
