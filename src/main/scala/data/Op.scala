package data

object Op {
  type Op[+R, -A] = A => R
}
