package chapter2

import scala.annotation.tailrec

object MyModule {

  def abs(n: Long): Long = if (n < 0) -n else n

  def factorial(n: Long): Long = if (n < 2) n else n * factorial(n - 1)

  //2.1
  def fib(n: Long): Long = {
    @tailrec
    def go(n: Long, a: Long, b: Long): Long = if (n < 2) a else go(n - 1, b, a + b)
    go(n, 0, 1)
  }

  def tailFactorial(n: Long): Long = {
    @tailrec
    def go(n: Long, acc: Long): Long = if (n < 1) acc else go(n - 1, n * acc)
    go(n, 1)
  }

  def format(s: String, n: Long, f: Long => Long) = s"The $s of $n is ${f(n)}"

  def formatAbs(n: Long) = s"The absolute value of $n is ${abs(n)}"

  def formatFactorial(n: Long) = s"The factorial of $n is ${tailFactorial(n)}"

  def formatFib(n: Long) = s"The fibonacci of $n is ${fib(n)}"

  //2.2
  def isSorted[A](a: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int, b: Boolean): Boolean = if (!b || n >= a.length) b else go(n + 1, ordered(a(n - 1), a(n)))
    go(1, true)
  }


  def isSortedFormat[A](a: Array[A], ordered: (A, A) => Boolean): String = {
    s"$a is${if (!isSorted[A](a, ordered)) " not " else " "}sorted"
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  //2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  //2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  //2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def main(args: Array[String]) = {
    val n = java.lang.Long.parseLong(args(0))
    List(
        ("absolute value", abs _),
        ("factorial", (abs _).andThen(tailFactorial)),
        ("fibonacci", (abs _).andThen(fib)))
      .map { case (s, f) => format(s, n, f) }
      .foreach(println)

    println(s"isSorted true == ${isSorted[Int](Array(0), _ <= _)}")
    println(s"isSorted true == ${isSorted[Int](Array(1, 2, 3), _ <= _)}")
    println(s"isSorted false == ${isSorted[Int](Array(1, 3, 2), _ <= _)}")
    println(s"isSorted true == ${isSorted[String](Array("hello", "world"), _ <= _)}")
  }
}
