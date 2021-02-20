package enheritance

trait RunnableOOP[T] {
  def run(t: T): String
}

case class Water(volume: Int)

case class Coffee(grade: String)

case object KettleOOP extends RunnableOOP[Water] {
  override def run(t: Water): String = s"The water is warming up, volume = ${t.volume} "
}

case object WaterFilterOOP extends RunnableOOP[Water] {
  override def run(t: Water): String = s"FilterWater, volume = ${t.volume} "
}

case object CoffeeMakerOOP extends RunnableOOP[Coffee] {
  override def run(t: Coffee): String = s"Make coffee, coffee grade = ${t.grade} "
}
