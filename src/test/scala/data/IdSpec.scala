package data

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class IdSpec extends AnyFlatSpec with should.Matchers {

  it should "id" in {
    import Id.Id
    import functor.Functor

    val idInt: Id[Int] = 10
    Functor.apply(Functor[Id]).map(idInt)(_ + 1) shouldBe 11
    Functor.apply(Functor[Id]).map(10)(_ + 1) shouldBe 11
  }
}
