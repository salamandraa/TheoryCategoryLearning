package enheritance

object PolymorphismFPvsOOP extends App {


  val coffeeMakerOOP: RunnableOOP[Coffee] = CoffeeMakerOOP
  println("OOP " + coffeeMakerOOP.run(Coffee("good")))
  println("FP  " + RunnableFP.run(Coffee("good")))
  println("FP  " + RunnableFP.run(Coffee("good"))(RunnableFP.RunnableLikeFp.CoffeeMakerFp))

  val seqRunnableOOPWater: Seq[RunnableOOP[Water]] = List(KettleOOP, WaterFilterOOP)
  val seqResultOOP = seqRunnableOOPWater.map(_.run(Water(200)))
  println("OOP " + seqResultOOP.mkString)

  val seqContextFP: Seq[RunnableFP.RunnableLikeFp[Water]] = List(RunnableFP.RunnableLikeFp.KettleFp, RunnableFP.RunnableLikeFp.WaterFilterFp)
  val seqResultFP = seqContextFP.map(context => RunnableFP.run(Water(200))(context))
  println("FP  " + seqResultFP.mkString)


  def printOOP[T](t: T, runnableOOP: RunnableOOP[T]): Unit = println("OOP " + runnableOOP.run(t))

  def printFp[T](t: T)(implicit context: RunnableFP.RunnableLikeFp[T]): Unit = println("FP  " + context.run(t))

  printOOP(Coffee("good"), CoffeeMakerOOP)
  printFp(Coffee("good"))
}
