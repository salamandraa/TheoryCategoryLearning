package data

import cats.Eq
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EqSpec extends AnyFlatSpec with should.Matchers {

  it should "test" in {

    // !!! bad !!! compare Option[Int] and Int !!!!!!
    List(1, 2, 3).map(Option(_)).filter(item => item == 1) shouldBe Nil

    import cats.syntax.eq._
    // use === or =!=
    Eq[Option[Int]].eqv(Some(1), None) shouldBe false
    Eq[Option[Int]].eqv(None, None) shouldBe true
    Eq[Option[Int]].eqv(Some(1), Some(1)) shouldBe true
    Eq[Option[Int]].eqv(Some(1), Some(2)) shouldBe false


    //compile errors
    //    Eq[Option[Int]].eqv(Some(1), 1) shouldBe true
    //    Eq[Option[Int]].eqv(1, Some(1)) shouldBe true

    import cats.syntax.option._

    1.some shouldBe Some(1)
    none[Int] shouldBe Option.empty[Int]

    implicit val eqCats: Eq[Cat] = Eq.instance(_ == _)


    val cat1 = Cat("Garfield",
      38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")
    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    Eq.eqv(cat1, cat2) shouldBe false
    Eq.eqv(cat1, cat1.copy()) shouldBe true
    Eq[Option[Cat]].eqv(optionCat1, optionCat2) shouldBe false
    Eq[Option[Cat]].eqv(optionCat1, optionCat1.map(_.copy())) shouldBe true


  }


}
