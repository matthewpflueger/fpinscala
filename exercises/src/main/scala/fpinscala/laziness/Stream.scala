package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: (A => Boolean)): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def take_not_good(n: Int): Stream[A] = {
    @annotation.tailrec
    def loop(i: Int, s: Stream[A], l: List[A]): Stream[A] = s match {
      case Cons(h, t) if i > 0 => loop(i - 1, t(), h() :: l)
      case _ => Stream(l.reverse: _*)
    }
    loop(n, this, Nil)
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def drop_not_good(n: Int): Stream[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], i: Int): Stream[A] = s match {
      case Cons(h, t) if (i > 0) => loop(t(), i - 1)
      case _ => s
    }
    loop(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def forAll_not_good(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
    case _ => true
  }

  def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startsWith(t2())
    case (_, Empty) => true
    case _ => false
  }


  def toList: List[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], l: List[A]): List[A] = s match {
      case Empty => l
      case Cons(h, t) => loop(t(), h() :: l)
    }
    loop(this, List()).reverse
  }



  def zipWithUnfold[B, C](s1: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s1)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2())), (t1(), t2()))
    case _ => None
  }

  def zipWith[B, C](s1: Stream[B])(f: (A, B) => C): Stream[C] = (this, s1) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Stream.cons(f(h1(), h2()), t1().zipWith(t2())(f))
    case _ => empty
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), empty))
    case (_, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
    case _ => None
  }

  //  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  def mapUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h1, t1) => Some((f(h1()), t1()))
    case _ => None
  }

  def takeUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h1, t1), num) if num > 0 => Some((h1(), (t1(), num - 1)))
    case _ => None
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h1, t1) if p(h1()) => Some((h1()), t1())
    case _ => None
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case _ => empty
  }

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def constantEfficient[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def fibs: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a, loop(b, a + b))
    }
    loop(0, 1)
  }

  def fibsUnfold: Stream[Int] = unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(a => Some((a, a)))

  val onesUnfold: Stream[Int] = constantUnfold(1)

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s+1)))
}