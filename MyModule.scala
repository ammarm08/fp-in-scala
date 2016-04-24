object MyModule {
  def abs(n: Int): Int = 
    if (n < 0) -n 
    else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(current: Int, first: Int, second: Int): Int =
      if (current >= n) second
      else go(current + 1, second, first + second)

    if (n <= 1) 0
    else if (n == 2) 1
    else go(1, 0, 1)
  }

  def binarySearch[A](as: Array[A], key: A, gt: (A, A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int =
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key, a)) mid2
        else if (greater) go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)
      }

    go(0, 0, as.length - 1)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int): Boolean =
      if (i == as.length) true
      else {
        val current = as(i)
        val previous = as(i-1)
        val greater = gt(previous, current)
        if (greater) false
        else go(i + 1)
      }

    go(1)
  }

  def partiall[A, B, C](a: A, f: (A, B) => C): B => C = {
    f(a: A, _: B)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => {
      (b: B) => f(a, b)
    }
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = 
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 5, factorial))
    println(formatResult("fibonacci value", 4, fibonacci))
    println(formatResult("incremented value", 10, x => x + 1))
}