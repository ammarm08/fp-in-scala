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

  def incrementEach(ints: List[Int]): List[Int] = {
    foldRight(ints, List.empty[Int])((x, acc) => x + 1 :: acc)
  }

  def stringify(doubles: List[Double]): List[String] = {
    foldRight(doubles, List.empty[String])((current, acc) => current.toString :: acc)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, List.empty[B])((current, acc) => f(current) :: acc)
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    foldLeft(l, List.empty[B])((acc, current) => acc ++ f(current))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, List.empty[A])((current, acc) => if (f(current)) current :: acc else acc)
  }

  def flatFilter[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)((x) => if (f(x)) List(x) else List.empty[A])
  }

  def constructList[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (xh :: xt, yh :: yt) => (f(xh, yh)) :: constructList(xt, yt)(f)
  }

  def take[A](l: List[A], n: Int): List[A] = (n, l) match {
    case (0, _) => Nil
    case (_, Nil) => Nil
    case (_, h :: t) => h :: take(t, n - 1)
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