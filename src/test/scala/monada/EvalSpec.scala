package monada

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class EvalSpec extends AnyFlatSpec with should.Matchers {

  it should "test" in {
    //eager + memoized // Now
    val x = {
      println("Computing X")
      math.random
    }
    println(x)
    println(x)

    println("start y")

    //lazy + not memoized // Always
    def y = {
      println("Computing Y")
      math.random
    }

    println(y)
    println(y)

    println("start z")
    //lazy + memoized //  Later
    lazy val z = {
      println("Computing Z")
      math.random
    }
    println(z)
    println(z)
    // !!!!!!!!!!   The final combination, eager and not memoized, is not possible. !!!!!!!!!!!!!!!!!!!!!!


    import cats.Eval
    println("Eval.now")
    val now = Eval.now(math.random + 1000)
    println(now.value)
    println(now.value)
    println("Eval.now")

    println("Eval.always")
    val always = Eval.always(math.random + 3000)
    println(always.value)
    println(always.value)
    println("Eval.always")

    println("Eval.later")
    val later = Eval.later(math.random + 2000)
    println(later.value)
    println(later.value)
    println("Eval.later")


    val greeting = Eval
      .always {
        println("Step 1");
        "Hello"
      }
      .map { str => println("Step 2"); s"$str world" }
    println("run greeting")
    println(greeting.value)
    println(greeting.value)
    println("run greeting")


    val ans = for {
      a <- Eval.now {
        println("Calculating A");
        40
      }
      b <- Eval.always {
        println("Calculating B");
        2
      }
    } yield {
      println("Adding A and B")
      a + b
    }
    println("run ans")
    println(ans.value)
    println(ans.value)
    println("run ans")


    val saying = Eval
      .always {
        println("Step 1");
        "The cat"
      }
      .map { str => println("Step 2"); s"$str sat on" }
      .memoize
      .map { str => println("Step 3"); s"$str the mat" }

    println("run saying")
    println(saying.value)
    println(saying.value)
    println("run saying")

    var aValue = 1

    val computation = for {
      b <- Eval.now { // порядок важен !!!! сначала now потом все остальные !!!
        println("calc b");
        2
      }
      a <- Eval.always {
        println("aValue");
        aValue
      }
    } yield a + b

    println("run computation")
    println(computation.value)
    aValue = 2
    println(computation.value)
    println("run computation")

    def factorial(n: BigInt): BigInt = if (n == 1) n else n * factorial(n - 1)

    //    println(factorial(50000).value) //java.lang.StackOverflowError

    def factorial1(n: BigInt): Eval[BigInt] = if (n == 1) Eval.now(n) else factorial1(n - 1).map(_ * n)

    //    println(factorial1(50000).value)  //java.lang.StackOverflowError
    def factorial2(n: BigInt): Eval[BigInt] = if (n == 1) Eval.now(n) else Eval.defer(factorial2(n - 1).map(_ * n))

    println(factorial2(50000).value)

    def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
      as match {
        case head :: tail =>
          fn(head, foldRight(tail, acc)(fn))
        case Nil =>
          acc
      }

    def foldRightSafety[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
      as match {
        case head :: tail => Eval.defer(foldRightSafety(tail, acc)(fn).map(b => fn(head, b)))
        case Nil => Eval.now(acc)
      }

    foldRightSafety(List(1, 2, 3), "")((a, b) => a.toString + b).value shouldBe "123"


    //    println(foldRight((1 to 100000).toList, 0L)(_ + _)) // java.lang.StackOverflowError
    println(foldRightSafety((1 to 100000).toList, 0L)(_ + _).value)
  }

}
