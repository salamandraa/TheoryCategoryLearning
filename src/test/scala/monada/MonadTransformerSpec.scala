package monada

import cats.data.{EitherT, OptionT}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

class MonadTransformerSpec extends AnyFlatSpec with should.Matchers {

  import cats.data.OptionT

  type ListOption[A] = OptionT[List, A]

  it should "test" in {
    //    import cats.instances.list._
    // for Monad
    import cats.syntax.applicative._ // for pure
    val result1: ListOption[Int] = OptionT.apply(List(Option(10)))
    // result1: ListOption[Int] = OptionT(List(Some(10)))
    val result2: ListOption[Int] = 32.pure[ListOption]
    // result2: ListOption[Int] = OptionT(List(Some(32)))
    result1.flatMap { (x: Int) =>
      result2.map { (y: Int) =>
        x + y
      }
    } shouldBe OptionT(List(Option(42)))

    {
      for {
        x <- result1
        y <- result2
      } yield x + y
    } shouldBe OptionT(List(Option(42)))

    type ListOptionOption[A] = OptionT[ListOption, A]

    10.pure[ListOptionOption] shouldBe OptionT(OptionT(List(Option(Option(10)))))

    import scala.concurrent.Future
    import cats.data.{EitherT, OptionT}
    type FutureEither[A] = EitherT[Future, String, A]
    type FutureEitherOption[A] = OptionT[FutureEither, A]

    import cats.instances.future._ // for Monad
    import scala.concurrent.Await
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    val futureEitherOr: FutureEitherOption[Int] =
      for {
        a <- 10.pure[FutureEitherOption]
        b <- 32.pure[FutureEitherOption]
      } yield a + b

    import cats.syntax.either._

    Await.result(futureEitherOr.value.value, Duration.Inf) shouldBe Option(42).asRight[String]


    import cats.data.Writer
    type Logged[A] = Writer[List[String], A]

    // Methods generally return untransformed stacks:
    def parseNumber(str: String): Logged[Option[Int]] =
      util.Try(str.toInt).toOption match {
        case Some(num) => Writer(List(s"Read $str"), Some(num))
        case None
        => Writer(List(s"Failed on $str"), None)
      }

    // Consumers use monad transformers locally to simplify composition:
    def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
      import cats.data.OptionT
      val result = for {
        a <- OptionT(parseNumber(a))
        b <- OptionT(parseNumber(b))
        c <- OptionT(parseNumber(c))
      } yield a + b + c
      result.value
    }
    import cats.data.WriterT

    // This approach doesn't force OptionT on other users' code:
    addAll("1", "2", "3") shouldBe Writer(List("Read 1", "Read 2", "Read 3"), Some(6))


    addAll("1", "a", "3") shouldBe Writer(List("Read 1", "Failed on a"), Option.empty[Int])

  }

  it should "exersise" in {

    type Response[A] = EitherT[Future, String, A]

    val powerLevels = Map(
      "Jazz" -> 6,
      "Bumblebee" -> 8,
      "Hot Rod" -> 10
    )
    import cats.implicits._

    def getPowerLevel(autobot: String): Response[Int] = powerLevels.get(autobot) match {
      case Some(value) => value.pure[Response]
      case None => EitherT.left(Future(s"Comms error: $autobot unreachable"))
    }

    def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = for {
      a <- getPowerLevel(ally1)
      b <- getPowerLevel(ally2)
    } yield a + b > 15

    def tacticalReport(ally1: String, ally2: String): String = {
      Try(Await.result(canSpecialMove(ally1, ally2).value, Duration.Inf)) match {
        case Failure(exception) => exception.toString
        case Success(either) => either match {
          case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
          case Right(false) => s"$ally1 and $ally2 need a recharge."
          case Left(value) => value
        }
      }
    }

    tacticalReport("Jazz", "Bumblebee") shouldBe "Jazz and Bumblebee need a recharge."
    tacticalReport("Bumblebee", "Hot Rod") shouldBe "Bumblebee and Hot Rod are ready to roll out!"
    tacticalReport("Jazz", "Ironhide") shouldBe "Comms error: Ironhide unreachable"


  }

}

