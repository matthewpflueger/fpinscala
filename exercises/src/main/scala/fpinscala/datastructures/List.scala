package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sumFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def productFoldLeft(ds: List[Double]): Double = foldLeft(ds, 0d)(_ + _)

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def flatten[A](l: List[List[A]]): List[A] = foldLeft(l, Nil: List[A])((ll, li) => append(ll, li))

  def appendViaFold[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a1i, a2l) => Cons(a1i, a2l))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def foldRightViaLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((z, a) => f(a, z))

  // Utility functions
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sumFoldRight(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def head[A](l: List[A]): A = l match {
    case Cons(x, _) => x
    case _ => sys.error("empty list")
  }

  def tail[A](l: List[A]): List[A] = drop(l, 1)

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, xs) => Cons(h, xs)
    case _ => sys.error("empty list")
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n < 1) l
    else l match {
      case Cons(_, xs) => drop(xs, n - 1)
      case _ => Nil
    }
  }

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if (f(x)) => dropWhile(xs)(f)
    case _ => l
  }

  def reverseFold[A](l: List[A]): List[A] = foldRight(l, Nil: List[A])(Cons(_, _))

  def reverse[A](l: List[A]): List[A] = {
    @tailrec
    def loop(acc: List[A], l: List[A]): List[A] = l match {
      case Cons(x, xs) => loop(Cons(x, acc), xs)
      case _ => acc
    }
    loop(Nil, l)
  }

  /**
   * Drop all elements that satisfy the predicate
   * @param l
   * @param f
   * @tparam A
   * @return
   */
  def dropAll[A](l: List[A])(f: A => Boolean): List[A] = {
    @tailrec
    def loop(acc: List[A], l: List[A]): List[A] = l match {
      case Cons(x, xs) if (f(x)) => loop(acc, xs)
      case Cons(x, xs) => loop(Cons(x, acc), xs)
      case _ => acc
    }
    reverse(loop(Nil, l))
  }

  def init[A](l: List[A]): List[A] = {
    @tailrec
    def loop(acc: List[A], l: List[A]): List[A] = l match {
      case Cons(x, a @ Cons(y, zs)) => loop(Cons(x, acc), a)
      case _ => acc
    }
    reverse(loop(Nil, l))
  }

  def length2[A](l: List[A]): Int = {
    @tailrec
    def loop(acc: Int, l: List[A]): Int = l match {
      case Cons(x, xs) => loop(acc + 1, xs)
      case _ => acc
    }
    loop(0, l)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0) { (_, i) => i + 1 }

  //ripped from companion book :(
  def foldLeftViaRight[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    case _ => z
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = reverse(foldLeft(l, Nil: List[B])((ll, li) => Cons(f(li), ll)))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = reverse(foldLeft(l, Nil: List[A]) { (acc, li) =>
    if (f(li)) Cons(li, acc) else acc
  })

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, Nil: List[B]) { case (b, a) => append(b, f(a)) }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) Cons(a, Nil) else Nil)

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    //loop through l, if head(l) == head(sub) then loop(tail(l), tail(sub)) else loop(tail(l), sub)
    //end loop if length(l) == 0 or length(sub) == 0
    @tailrec
    def loop(lt: List[A], subt: List[A]): Boolean = (lt, subt) match {
      case (Nil, Cons(subh, subtt)) => false
      case (_, Nil) => true
      case (Cons(lh, ltt), Cons(subh, subtt)) if (lh == subh) => loop(ltt, subtt)
      case (Cons(lh, ltt), _) => loop(ltt, sub)
    }
    loop(l, sub)
  }

}
