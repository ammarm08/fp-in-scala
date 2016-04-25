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

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case h :: t => h :: append(t, l2)
  }

  def init[A](l: List[A]): List[A] = l match {
    case i if i.length <= 1 => Nil
    case h :: t => h :: (init(t))
  }

  def sum(ints: List[Int]): Int = {
    foldRight(ints, 0)(_ + _)
  }

  def product(doubles: List[Double]): Double = {
    foldRight(doubles, 1.0)(_ * _)
  }

  def foldRight[A, B](l: List[A], acc: B)(f: (A, B) => B): B = l match {
    case Nil => acc
    case h :: t => f(h, foldRight(t, acc)(f))
  }
}