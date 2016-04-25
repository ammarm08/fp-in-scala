sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth(tree: Tree[Int]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](tree: Tree[A])(base: A => B)(recur: (B, B) => B): B = tree match {
    case Leaf(n) => base(n)
    case Branch(l, r) => recur(fold(l)(base)(recur), fold(r)(base)(recur))
  }

  def sizeFold[A](tree: Tree[A]): Int = {
    fold(tree)(x => 1)(1 + _ + _)
  }

  def maximumFold(tree: Tree[Int]): Int = {
    fold(tree)(x => x)(_ max _)
  }

  def depthFold(tree: Tree[Int]): Int = {
    fold(tree)(x => 0)(1 + (_ max _))
  }

  def mapFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(x => Leaf(f(x)): Tree[B])(Branch(_,_))
  }
}
