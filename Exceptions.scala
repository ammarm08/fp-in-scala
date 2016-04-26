import java.util.regex._

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  // return Some case, or else default if undefined
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  // map and return the result, or else return None if undefined
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  // return first option, or else return default option
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => this
    case _ => None
  }

  def mean(nums: Seq[Double]): Option[Double] =
    if (nums.isEmpty) None
    else Some(nums.sum / nums.length)

  // map the variance of each num in the sequence 
  def variance(nums: Seq[Double]): Option[Double] =
    mean(nums) flatMap (m => mean(nums.map(x => math.pow(x - m, 2))))

  // comparing two objects lifted from their Option containers, with predicate f
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aOption => b map (bOption => f(aOption, bOption)))

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

/* pattern:
** try to regex compile string into an Option[Pattern]
** if err, catch error and return None
*/
def pattern(s: String): Option[Pattern] =
  try {
    Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

/* container for pattern matcher
** pattern(pat) returns Option[Pattern], and
** map lifts Pattern out of Option and returns a function (String => Boolean)
** contained within an Option type
*/
def mkMatcher(pat: String): Option[String => Boolean] =
  pattern(pat) map (p => (s: String) => p.matcher(s).matches)

// same as above, except uses a for-comprehension
def mkMatcher_comp(pat: String): Option[String => Boolean] =
  for {
    p <- pattern(pat)
  } yield ((s: String) => p.matcher(s).matches)

// then you can invoke that returned function using a for-comprehension
def doesMatch(pat: String, s: String): Option[Boolean] =
  for {
    p <- mkMatcher_comp(s) // p is the matching function we constructed
  } yield p(s) // invoke p with s

// does input string match both patterns?
// mkMatcher returns Option[Pattern] (pat and pat2)
// p -> lifted pattern matching function for pat
// q -> lifted pattern matching function for pat2
// p(s) && q(s) -> does input string match both patterns?
def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
  mkMatcher(pat) flatMap (p => mkMatcher(pat2) map (q => (p(s) && q(s))))

// TO DO: finish this.
// def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
//   map2(mkMatcher(pat1), mkMatcher(pat2))

// given a list of options, return an option with list of lifted values
def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
  case Nil => Some(Nil)
  case h :: t => h flatMap (hOption => sequence(t) map(hOption :: _))
}

// given a list of strings, return an option with list of lifted pattern functions
// `a map pattern` -> builds list of Option[Pattern]
// passing above list to sequence -> Option[List[Pattern]]
// this isn't efficient because it traverses list twice
def parsePatterns(a: List[String]): Option[List[Pattern]] =
  sequence(a map pattern)

// given list and option transform function, return option[list]
// base case -> return some(nil)
// recursive case -> use option transform function to transform head ...
// ... and then recur on tail until we hit a nil case ...
// ... which unfolds the recursion, concatenating option onto option list
def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case Nil => Some(Nil)
  case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
}
  

