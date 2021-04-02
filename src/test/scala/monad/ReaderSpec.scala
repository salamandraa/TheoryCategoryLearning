package monada

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ReaderSpec extends AnyFlatSpec with should.Matchers {

  import cats.data.Reader

  it should "test" in {

    import cats.data.Reader
    final case class Cat(name: String, favoriteFood: String)
    val catName: Reader[Cat, String] =
      Reader(cat => cat.name)

    catName.run(Cat("Garfield", "lasagne")) shouldBe "Garfield"

    val greetKitty: Reader[Cat, String] =
      catName.map(name => s"Hello ${name}")

    val feedKitty: Reader[Cat, String] =
      Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

    val greetAndFeed: Reader[Cat, String] =
      for {
        greet <- greetKitty
        feed
          <- feedKitty
      } yield s"$greet. $feed."

    greetAndFeed(Cat("Garfield", "lasagne")) shouldBe "Hello Garfield. Have a nice bowl of lasagne."

    greetAndFeed(Cat("Heathcliff", "junk food")) shouldBe "Hello Heathcliff. Have a nice bowl of junk food."

    final case class Db(
                         usernames: Map[Int, String],
                         passwords: Map[String, String]
                       )
    type DbReader[R] = Reader[Db, R]

    def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))

    def checkPassword(
                       username: String,
                       password: String): DbReader[Boolean] = Reader(db => db.passwords.get(username).contains(password))

    import cats.syntax.applicative._
    def checkLogin(
                    userId: Int,
                    password: String): DbReader[Boolean] = {
      for {
        userNameOpt <- findUsername(userId)
        result <- userNameOpt.map(checkPassword(_, password)).getOrElse(false.pure[DbReader])
      } yield result
    }

    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )
    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )
    val db = Db(users, passwords)
    checkLogin(1, "zerocool").run(db) shouldBe true

    checkLogin(4, "davinci").run(db) shouldBe false


  }
}
