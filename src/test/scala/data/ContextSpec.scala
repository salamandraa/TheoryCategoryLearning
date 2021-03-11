package data

import cats.effect.{ContextShift, IO}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.concurrent.{ExecutionContext, Future}

object ContextSpec {

  sealed trait Context[F[+_]]

  object Context {
    private case class ExecutionContextImpl(context: ExecutionContext) extends Context[Future]

    implicit class ExecutionContextImplOps(context: ExecutionContext) {
      def toContextFuture: Context[Future] = ExecutionContextImpl(context)
    }

    implicit def toExecutionContext(implicit context: Context[Future]): ExecutionContext = context match {
      case ExecutionContextImpl(context) => context
      case _ => ???
    }

    private case class ContextShiftIoImpl(context: ContextShift[IO]) extends Context[IO]

    implicit class ContextShiftIoImplOps(context: ContextShift[IO]) {
      def toContextIO: Context[IO] = ContextShiftIoImpl(context)
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

    implicit val executionContextImpl: Context[Future] = scala.concurrent.ExecutionContext.global.toContextFuture

    def printExecutionContext(implicit executionContext: ExecutionContext): Unit = println(executionContext)

    printExecutionContext

    implicit val contextShiftIoImpl: Context[IO] = cats.effect.IO.contextShift(scala.concurrent.ExecutionContext.global).toContextIO

    def printContextShiftIo(implicit context: ContextShift[IO]): Unit = println(context)

    printContextShiftIo

  }

}
