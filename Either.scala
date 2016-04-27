sealed trait Either[E+, A+] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either(EE, B)): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => f(a)
  }

  def map2[EE >: E, B](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      a <- this
      b1 <- b
    } yield f(a, b1)
  }
}

// `Either` is a disjoint union
case class Left[E+](value: E) extends Either[E, Nothing]
case class Right[E+](value: A) extends Either[Nothing, A]

// given a list and a transform function, return a type Either with Left(e) and Right(list)
// transform --> (h: A) => f(h): Either[E, B]
// map --> (transformed head, traversed tail) -> head :: traversed tail
def traverse[E,A,B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = a match {
  case Nil => Right(Nil)
  case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
}

// given a list of Eithers, return a type Either with Left(e) and Right(list)
def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
  traverse(a)(x => x)