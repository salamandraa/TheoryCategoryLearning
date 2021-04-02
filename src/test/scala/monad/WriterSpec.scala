package monada

import functor.Functor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class WriterSpec extends AnyFlatSpec with should.Matchers {

  import cats.data.Writer

  it should "test" in {

    val vector = Writer(Vector(
      "It was the best of times",
      "it was the worst of times"
    ), 1859)


    //    import cats.instances.vector._
    109
    // for Monoid
    import cats.syntax.applicative._ // for pure
    type Logged[A] = Writer[Vector[String], A]
    val pureA = 123.pure[Logged]

    pureA.value shouldBe 123
    pureA.written shouldBe Vector()

    import cats.syntax.writer._

    val pureLog = Vector("msg1", "msg2", "msg3").tell

    pureLog.written shouldBe Vector("msg1", "msg2", "msg3")

    val a1 = Writer(Vector("msg1", "msg2", "msg3"), 123)
    a1.value shouldBe 123
    a1.written shouldBe Vector("msg1", "msg2", "msg3")
    a1.run shouldEqual(Vector("msg1", "msg2", "msg3"), 123)

    val b1 = 123.writer(Vector("msg1", "msg2", "msg3"))
    b1.value shouldBe 123
    b1.written shouldBe Vector("msg1", "msg2", "msg3")
    b1.run shouldEqual(Vector("msg1", "msg2", "msg3"), 123)

    val writer1 = for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b", "c").tell
      b <- 32.writer(Vector("x", "y", "z"))
    } yield a + b

    writer1.run shouldBe(Vector("a", "b", "c", "x", "y", "z"), 42)


    10.pure[Logged].flatMap {
      aVal =>
        Vector("a", "b", "c").tell.flatMap { _ =>
          32.writer(Vector("x", "y", "z")).map { b => aVal + b }
        }
    }.run shouldBe(Vector("a", "b", "c", "x", "y", "z"), 42)


    writer1.mapWritten(_.map(_.toUpperCase)).run shouldBe(Vector("A", "B", "C", "X", "Y", "Z"), 42)

    val writer3 = writer1.bimap(
      log => log.map(_.toUpperCase),
      res => res * 100
    )
    writer3.run shouldBe(Vector("A", "B", "C", "X", "Y", "Z"), 4200)

    val writer4 = writer1.mapBoth { (log, res) =>
      val log2 = log.map(_.toUpperCase)
      val res2 = res * 1000
      (log2, res2)
    }

    writer4.run shouldBe(Vector("A", "B", "C", "X", "Y", "Z"), 42000)

    writer1.reset.run shouldBe(Vector(), 42)
    writer1.swap.run shouldBe(42, Vector("a", "b", "c", "x", "y", "z"))


    def slowly[A](body: => A): A =
      try body finally Thread.sleep(100)


    def factorial(n: Int): Int = {
      val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
      println(s"fact $n $ans")
      ans
    }

    def factorialWriter(n: Int): Writer[Vector[String], Int] = {
      val ansLog = slowly(if (n == 0) 1.pure[Logged] else factorialWriter(n - 1).map(_ * n))
      for {
        ans <- ansLog
        _ <- Vector(s"fact $n $ans").tell
      } yield ans
    }

    factorialWriter(5).run shouldBe(Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6", "fact 4 24", "fact 5 120"), 120)

    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits._
    import scala.concurrent.duration._

    val res = Await.result(Future.sequence(Vector(
      Future(factorialWriter(5)),
      Future(factorialWriter(5))
    )), 5.seconds)

    // all ok with async logging
    res.map(_.run) shouldBe Vector(
      (Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6", "fact 4 24", "fact 5 120"), 120),
      (Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6", "fact 4 24", "fact 5 120"), 120))


  }

}
