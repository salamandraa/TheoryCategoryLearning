package apply

import cats.{Apply, Functor, Invariant, Monad}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SemigroupalAndApplicativeSpec extends AnyFlatSpec with should.Matchers {

  import cats.Semigroupal

  it should "test" in {
    Semigroupal[Option].product(Some(123), Some("abc")) shouldBe Some((123, "abc"))
    Semigroupal[Option].product(None, Some("abc")) shouldBe None
    Semigroupal[Option].product(Some(123), None) shouldBe None

    Semigroupal.tuple3(Option(1), Option(2), Option(3)) shouldBe Some((1, 2, 3))
    Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int]) shouldBe None

    Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _) shouldBe Some(6)
    Semigroupal.map2(Option(1), Option.empty[Int])(_ + _) shouldBe None

    //    Semigroupal.contramap2()
    Semigroupal.imap2(Option(1), Option.empty[Int])(_ + _)((_, 0)) shouldBe None
    //    product(a, product(b, c)) == product(product(a, b), c)


    import cats.syntax.apply._ //     for tupled and mapN


    (Option(123), Option("abc")).tupled shouldBe Option((123, "abc"))
    (Option(123), Option("abc"), Option(true)).tupled shouldBe Option((123, "abc", true))

    final case class Cat(name: String, born: Int, color: String)

    (Option("Garfield"), Option(1978), Option("Orange & black")).mapN(Cat.apply) shouldBe Some(Cat("Garfield", 1978, "Orange & black"))
  }

  it should "test2" in {
    import cats.Monoid
    import cats.instances.invariant._ // for Semigroupal
    import cats.syntax.apply._ // for imapN


    final case class Cat(name: String,
                         yearOfBirth: Int,
                         favoriteFoods: List[String])

    val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply _
    val catToTuple: Cat => (String, Int, List[String]) = cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

    implicit val catMonoid: Monoid[Cat] = (Monoid[String], Monoid[Int], Monoid[List[String]]).imapN(tupleToCat)(catToTuple)


    def functorFromInvariant[T[_]](implicit invariant: Invariant[T]): Functor[T] = new Functor[T] {
      override def map[A, B](fa: T[A])(f: A => B): T[B] = invariant.imap(fa)(f)(b => ???)
    }

    val catMonoid2: Monoid[Cat] = (Monoid[String], Monoid[Int], Monoid[List[String]]).mapN(Cat.apply)(functorFromInvariant[Monoid], Semigroupal[Monoid])

    val catMomoid3: Monoid[Cat] = new Monoid[Cat] {
      override def empty: Cat = Cat(Monoid.empty[String], Monoid.empty[Int], Monoid.empty[List[String]])

      override def combine(x: Cat, y: Cat): Cat = Cat(Monoid.combine(x.name, y.name), Monoid.combine(x.yearOfBirth, y.yearOfBirth), Monoid.combine(x.favoriteFoods, y.favoriteFoods))
    }

    import cats.syntax.semigroup._ // for |+|
    val garfield = Cat("Garfield", 1978, List("Lasagne"))
    val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))
    garfield.|+|(heathcliff) shouldBe Cat("GarfieldHeathcliff", 1978 + 1988, List("Lasagne", "Junk Food"))
    Monoid[Cat].empty shouldBe Cat("", 0, List())
    catMonoid2.empty shouldBe Cat("", 0, List())
    catMomoid3.empty shouldBe Cat("", 0, List())


    import cats.instances.list
    Semigroupal[List].product(List(1, 2), List(3, 4)) shouldBe List((1, 3), (1, 4), (2, 3), (2, 4))

    type ErrorOr[A] = Either[Vector[String], A]
    Semigroupal[ErrorOr].product(
      Left(Vector("Error 1")),
      Left(Vector("Error 2"))
    ) shouldBe Left(Vector("Error 1"))


    (List(1, 2), List(3, 4)).tupled shouldBe List((1, 3), (1, 4), (2, 3), (2, 4))
    (List(1, 2), List(3, 4)).apWith(List((l: Int, r: Int) => (l, r))) shouldBe List((1, 3), (1, 4), (2, 3), (2, 4))
  }

  it should "test3" in {


    import cats.Semigroupal
    import cats.instances.either._ // for Semigroupal
    type ErrorOr[A] = Either[Vector[String], A]
    val error1: ErrorOr[Int] = Left(Vector("Error 1"))
    val error2: ErrorOr[Int] = Left(Vector("Error 2"))
    import cats.syntax.apply._ // for tupled
    import cats.instances.vector._ // for Semigroup on Vector
    (error1, error2).tupled shouldBe Left(Vector("Error 1"))

    import cats.syntax.parallel._ // for parTupled

    import cats.syntax.parallel._ // for parTupled
    (error1, error2).parTupled shouldBe Left(Vector("Error 1", "Error 2"))
    (List(1, 2), List(3, 4)).parTupled shouldBe List((1, 3), (2, 4))
    (List(1, 2), List(3, 4)).tupled shouldBe List((1, 3), (1, 4), (2, 3), (2, 4))

    import cats.instances.list._ // for Semigroup on List
    type ErrorOrList[A] = Either[List[String], A]
    val errStr1: ErrorOrList[Int] = Left(List("error 1"))
    val errStr2: ErrorOrList[Int] = Left(List("error 2"))
    (errStr1, errStr2).parTupled shouldBe Left(List("error 1", "error 2"))


    val success1: ErrorOr[Int] = Right(1)
    val success2: ErrorOr[Int] = Right(2)
    val addTwo = (x: Int, y: Int) => x + y

    (error1, error2).parMapN(addTwo) shouldBe Left(Vector("Error 1", "Error 2"))
    (success1, success2).parMapN(addTwo) shouldBe Right(3)

    (error1, error2).mapN(addTwo) shouldBe Left(Vector("Error 1"))
    (success1, success2).mapN(addTwo) shouldBe Right(3)


  }

  // implements Apply for List Try Option
  // check parMap and maybe impliment

  //Monoid

  def empty[A]: A = ??? //Monoid

  def combine[A](x: A, y: A): A = ??? // Semigroup

  // Applicative
  def pureF[F[_], A](x: A): F[A] = ??? // Applicative

  def ap[F[_], A, B](ff: F[A => B])(fa: F[A]): F[B] = mapF(product(fa, ff))(aPairAtoB => aPairAtoB._2(aPairAtoB._1)) // Apply

  def product[F[_], A, B](fa: F[A], fb: F[B]): F[(A, B)] = ap(mapF(fa)(a => (b: B) => (a, b)))(fb) // Semigroupal

  def mapF[F[_], A, B](fa: F[A])(f: A => B): F[B] = ap(pureF(f))(fa) // Functor

  //Monad
  def pureM[F[_], A](x: A): F[A] = ??? // Applicative

  def flatMap[F[_], A, B](fa: F[A])(f: A => F[B]): F[B] = flatten(mapM(fa)(f)) //FlatMap

  def flatten[F[_], A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity) //FlatMap

  def mapM[F[_], A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pureM(f(a))) // Functor


  val myApplyOption: Apply[Option] = new Apply[Option] {
    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = ff -> fa match {
      case (Some(f), Some(fa)) => Some(f(fa))
      case _ => None
    }

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  Apply[Option].ap(Some((x: Int) => (x * 2).toString))(Some(10)) shouldBe Some("20")
  myApplyOption.ap(Some((x: Int) => (x * 2).toString))(Some(10)) shouldBe Some("20")

  val myApplyList: Apply[List] = new Apply[List] {
    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = ff.flatMap(f => fa.map(a => f(a)))

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }


  Apply[List].ap(List((x: Int) => (x * 2).toString, (x: Int) => (x + 1).toString))(List(1, 2, 3)) shouldBe List("2", "4", "6", "2", "3", "4")
  myApplyList.ap(List((x: Int) => (x * 2).toString, (x: Int) => (x + 1).toString))(List(1, 2, 3)) shouldBe List("2", "4", "6", "2", "3", "4")

  cats.instances.list

}
