package data1

case class Reader[-T, +R](fun: T => R) {
  def apply(t: T): R = fun(t)
}
