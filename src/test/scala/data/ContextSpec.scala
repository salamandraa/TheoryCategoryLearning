package data

import cats.effect.{ContextShift, IO}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.concurrent.{ExecutionContext, Future}

object ContextSpec {

  sealed trait Context[F[+_]]

  case class ExecutionContextImpl(context: ExecutionContext) extends Context[Future]

  case class ContextShiftIoImpl(context: ContextShift[IO]) extends Context[IO]

  //  case class ContextShiftImpl[F[_]](context: ContextShift[F]) extends Context[F]

  object Context {


    implicit def toExecutionContext(implicit context: Context[Future]): ExecutionContext = context match {
      case ExecutionContextImpl(context) => context
      case _ => ???
    }

    implicit def toContextShift(implicit context: Context[IO]): ContextShift[IO] = context match {
      case ContextShiftIoImpl(context) => context
      case _ => ???
    }

  }

}

class ContextSpec extends AnyFlatSpec with should.Matchers {

  import ContextSpec._

  it should "test" in {

    import Context._

    implicit val executionContextImpl: Context[Future] = ExecutionContextImpl(scala.concurrent.ExecutionContext.global)

    def printExecutionContext(implicit executionContext: ExecutionContext): Unit = println(executionContext)

    printExecutionContext


    implicit val contextShiftIoImpl: Context[IO] = ContextShiftIoImpl(cats.effect.IO.contextShift(scala.concurrent.ExecutionContext.global))

    def printContextShiftIo(implicit context: ContextShift[IO]): Unit = println(context)

    printContextShiftIo

  }

}
