package data

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ConstSpec extends AnyFlatSpec with should.Matchers {

  it should "const" in {
    import functor.Functor

    Functor[Const[Int, *]].map(Const(10))(_ => "") shouldBe Const(10)
    Functor[Const[Unit, *]].map(Const(()))(_ => "") shouldBe Const(())
  }
}
