package enheritance

object RunnableFP {
  /**
   * fun for public using
   */
  def run[T](t: T)(implicit context: RunnableLikeFp[T]): String = context.run(t)

  trait RunnableLikeFp[T] {
    def run(t: T): String

  }

  object RunnableLikeFp {

    implicit object KettleFp extends RunnableLikeFp[Water] {
      override def run(t: Water): String = s"The water is warming up, volume = ${t.volume} "
    }

    implicit object WaterFilterFp extends RunnableLikeFp[Water] {
      override def run(t: Water): String = s"FilterWater, volume = ${t.volume} "
    }

    implicit object CoffeeMakerFp extends RunnableLikeFp[Coffee] {
      override def run(t: Coffee): String = s"Make coffee, coffee grade = ${t.grade} "
    }

  }

}

