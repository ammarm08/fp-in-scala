object DS {
  def tail[A](l: List[A]) = l match {
    case Nil => Nil
    case h :: t => t // the double colon is scala's concat
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case h :: t if f(h) => dropWhile(t, f)
    case _ => l
  }

  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Nil => Nil
    case h :: t => x :: t
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)((h, acc) => h :: acc)
  }

  def flatten[A](lists: List[List[A]]): List[A] = {
    foldRight(lists, List.empty[A])(append)
  }

  def init[A](l: List[A]): List[A] = l match {
    case i if i.length <= 1 => Nil
    case h :: t => h :: (init(t))
  }

  def sum(ints: List[Int]): Int = {
    foldLeft(ints, 0)(_ + _)
  }

  def product(doubles: List[Double]): Double = {
    if (doubles.length == 0) 0
    else foldLeft(doubles, 1.0)(_ * _)
  }

  def length[A](l: List[A]): Int = {
    foldLeft(l, 0)((acc, _) => acc + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    def go(as: List[A], acc: List[A]): List[A] = {
      foldLeft(l, acc)((x, y) => y :: x)
    }
    go(l, Nil)
  }

  // not TCO-ed, will stackoverflow for large lists
  def foldRight[A, B](l: List[A], acc: B)(f: (A, B) => B): B = l match {
    case Nil => acc
    case h :: t => f(h, foldRight(t, acc)(f))
  }

  // tail-call optimized
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(as: List[A], acc: B): B = as match {
      case Nil => acc
      case h :: t => go(t, f(acc, h))
    }
    go(l, z)
  }
}