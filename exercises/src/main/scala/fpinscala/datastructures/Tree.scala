package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[_](t: Tree[_]): Int = fold(t, 0)((b, _) => b + 1)

  def maximum(t: Tree[Int]): Int = fold(t, Int.MinValue)(_ max _)

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(t1, t2) => Branch(map(t1)(f), map(t2)(f))
  }

  def depth[_](t: Tree[_]): Int = {
    def loop(t: Tree[_], depth: Int): Int = t match {
      case Leaf(_) => depth + 1
      case Branch(t1, t2) => loop(t1, depth + 1) max loop(t2, depth + 1)
    }
    loop(t, 0)
  }

  def fold[A, B](t: Tree[A], z: B)(f: (B, A) => B): B = t match {
    case Leaf(a) => f(z, a)
    case Branch(t1, t2) => fold(t2, fold(t1, z)(f))(f)
  }

}