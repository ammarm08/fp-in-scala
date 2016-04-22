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