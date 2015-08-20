package chapter2

import scala.annotation.tailrec

object MyModule {

  def abs(n: Int) = if (n < 0) -n else n

  def factorial(n: Int): Long = if (n < 2) n else n * factorial(n - 1)

  def fib(n: Int): Long = {
    @tailrec
    def go(n: Int, a: Int, b: Int): Long = if (n < 2) a else if (n < 3) b else go(n - 1, b, (a + b))
    go(n, 0, 1)
  }

  def tailFactorial(n: Int): Long = {
    @tailrec
    def go(n: Int, acc: Int): Long = if (n < 1) acc else go(n - 1, n * acc)
    go(n, 1)
  }

  def formatAbs(n: Int) = s"The absolute value of $n is ${abs(n)}"

  def formatFactorial(n: Int) = s"The factorial of $n is ${tailFactorial(n)}"

  def formatFib(n: Int) = s"The fibonacci of $n is ${fib(n)}"

  def main(args: Array[String]) = {
    val n = Integer.parseInt(args(0))
    List(formatAbs _, (abs _).andThen(formatFactorial _), (abs _).andThen(formatFib _)).map(_(n)).foreach(println)
  }
}
