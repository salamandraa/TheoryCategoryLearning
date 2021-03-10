package data

import cats.Show
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.{should, _}

class PrintableSpec extends AnyFlatSpec with should.Matchers {

  import PrintableSyntax._

  implicit val printableCat: Printable[Cat] = new Printable[Cat] {
    override def format(value: Cat): String = s"${Printable.format(value.name)} is a ${value.age.format} year-old ${Printable.format(value.color)} cat."
  }

  it should "test" in {

    val cat = Cat("Salem", 1000, "Black")

    Printable.format(cat) shouldBe "Salem is a 1000 year-old Black cat."
    Printable.print(cat)


    // using PrintableSyntax

    cat.format shouldBe "Salem is a 1000 year-old Black cat."
    cat.print

    import cats.syntax.show._
    println("hello world".show)

    // using Show

    implicit val showCat: Show[Cat] = Show.show(cat => s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat.")

    cat.show shouldBe "Salem is a 1000 year-old Black cat."


  }

}