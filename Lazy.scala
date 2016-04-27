trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  // not tail recursive
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  // in tail position
  def toListTailRec: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]):: List[A] = s match {
      case Cons(h, t) => h() :: acc
      case _ => acc
    }

    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeWhile(f: A => Boolean) = this match {
    case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }
}

object Stream {

  def empty[A]: Stream[A] = 
    new Stream[A] { def uncons = None }

  def cons[A](head: => A, tail: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((head, tail))
    }

  // non-strict second param for f (see `=> B`)
  // if uncons-ing the stream -> Some => return function f w/ params h and recursive fold on tail
  // if uncons -> None => return function z (base case)
  // lazy because: only evaluates z if None, and only evaluates f if Some
  def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons match {
    case Some((h, t)) => f(h, t.foldRight(z)(f))
    case None => z
  }

  // infinite stream
  // lazy val tail --> an infinitely extended Cons-ing of `a` onto previous tail
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // infinite stream counting up by one. `cons` -> new stream of head n and tail from(n+1)
  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  // infinite fibonacci stream
  val fibs = {
    def go(p: Int, q: Int): Stream[Int] =
      cons(p, go(q, p+q))
    go(0, 1)
  }

  // f(z) takes type S and transforms it to Option[(A, S)]
  // "cotermination", "corecursion", "guarded recursion"
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =  f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  // execute `z` func, then execs `f` via pattern match.
  val fibsUnfold = unfold((0,1)) {
    case (p, q) => Some((p, (q, p+q)))
  }

  def fromUnfold(n: Int) = {
    unfold(n)(n => Some((n, n+1))
  }

  def constantUnfold[A](a: A) = {
    unfold(a)(_ => Some((a, a)))
  }

  def mapUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }
  }

  def takeUnfold[A](n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h,t), 1) => Some((h(), (empty, 0)))
      case (Cons(h,t), n) => Some((h(), (t(), n-1)))
      case _ => None
    }
  }

  def takeWhileUnfold[A](f: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }
  }


  // def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = (l) match {
  //   case Nil => false
  //   case h :: t if (take(t, sub.length) == sub) => true
  //   case h :: t => hasSubsequence(t, sub)
  // }

  // lazy evaluation
  // if None -> false right away, no other evaluation
  // else -> predicate check head, or if not valid, check rest of tail
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // lazy eval
  // if None -> false
  // else predicate check on head AND rest of tail, false if any eval to false
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // lazy eval
  // if None -> Empty
  // else predicate check. if true, build stream else empty stream
  def takeWhile2(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else empty)

  // apply f on h, then cons onto accumulating Stream
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  // only cons h onto stream if it passes predicate else leave acc unchanged
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  // cons each head 
  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  // append each streamified head onto tail
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}