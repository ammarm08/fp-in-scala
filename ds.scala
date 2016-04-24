package fpinscala.datastructures

// +A -> type A is covariant (could be Int, could be String, etc)
sealed trait List[+A]

// default empty
case object Nil extends List[Nothing]

// {head: x, y: list }
case class Cons[+A](head: A, tail: List[A] extends List[A])


// companion to trait List ("prototype" equivalent in JS speak)
object List {

  // recursive: int + sum(list) until nil
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  // recursive: int + product(list)
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case (0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // variadic function syntax (for undefined # of parameters)
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}