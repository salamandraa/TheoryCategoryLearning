package data

import data.Printable.format

trait Printable[-A] {
  def format(value: A): String
}

object Printable {
  //  1.3 Exercise: Printable Library
  def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)

  def print[A](value: A)(implicit printable: Printable[A]): Unit = println(format(value))

  implicit val printableInt: Printable[Int] = (value: Int) => value.toString
  implicit val printableString: Printable[String] = (value: String) => value
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]): String = printable.format(value)

    def print(implicit printable: Printable[A]): Unit = println(format)
  }
}