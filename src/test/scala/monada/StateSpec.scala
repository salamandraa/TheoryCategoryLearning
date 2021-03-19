package monada

import cats.data.State
import cats.implicits.catsSyntaxApplicativeId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Success, Try}

class StateSpec extends AnyFlatSpec with should.Matchers {


  import cats.data.State


  it should "test" in {
    val a = State[Int, String] { state =>
      (state + 1, s"The old state is $state")
    }
    a.run(1).value shouldBe(2, "The old state is 1")
    a.runS(1).value shouldBe 2
    a.runA(1).value shouldBe "The old state is 1"


    val step1 = State[Int, String] { num =>
      val ans = num + 1
      (ans, s"Result of step1: $ans")
    }
    val step2 = State[Int, String] { num =>
      val ans = num * 2
      (ans, s"Result of step2: $ans")
    }
    val both = for {
      a <- step1
      b <- step2
    } yield (a, b)
    both.run(20).value shouldBe(42, ("Result of step1: 21", "Result of step2: 42"))



    // get state + duplicated A as S
    State.get[Int].run(10).value shouldBe(10, 10)

    // set state + Unit
    State.set[Int](30).run(10).value shouldBe(30, ())

    // get state + A
    State.pure[Int, String]("Result").run(10).value shouldBe(10, "Result")

    // get state + A from state
    State.inspect[Int, String](x => s"$x!").run(10).value shouldBe(10, "10!")

    // change state + Unit
    State.modify[Int](_ + 1).run(10).value shouldBe(11, ())


    import State._
    val program1: State[Int, (Int, Int, Int)] = for {
      a <- get[Int]
      _ <- set[Int](a + 1)
      b <- get[Int]
      _ <- modify[Int](_ + 1)
      c <- inspect[Int, Int](_ * 1000)
    } yield (a, b, c)

    program1.run(1).value shouldBe(3, (1, 2, 3000))


    type CalcState[A] = State[List[Int], A]

    def evalOne(sym: String): CalcState[Int] = State[List[Int], Int] { oldStack =>
      val newStack = Try(sym.toInt) match {
        case Success(int) => int :: oldStack
        case _ =>
          val snd :: frs :: tail = oldStack
          val resultOperator = sym match {
            case "+" => frs + snd
            case "-" => frs - snd
            case "*" => frs * snd
            case "/" => frs / snd
          }
          resultOperator :: tail
      }
      newStack -> newStack.head
    }

    evalOne("42").runA(Nil).value shouldBe 42

    val program = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    } yield ans

    program.runA(Nil).value shouldBe 3

    def evalAll(input: List[String]): CalcState[Int] = input.foldLeft(0.pure[CalcState]
    )((acc, value) => acc.flatMap(_ => evalOne(value)))
    //      input match {
    //      case ::(head, next) => evalOne(head).flatMap(_ => evalAll(next))
    //      case Nil => State[List[Int], Int] { s => s -> s.head }
    //    }


    val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))

    multistageProgram.runA(Nil).value shouldBe 9

    val biggerProgram = for {
      _ <- evalAll(List("1", "2", "+"))
      _ <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans

    biggerProgram.runA(Nil).value shouldBe 21
  }

}
