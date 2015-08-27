package fpinscala.datastructures

import utest.TestSuite
import utest._

object ListTest extends TestSuite {
  val tests = TestSuite {
    'dropAll {
      val l = List.dropAll(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil)))))))(x => x % 2 == 1)
      assert(List.length(l) == 3)
      assert(List.head(l) == 2)
      assert(List.dropAll(l){ x => x % 2 == 0 } == Nil)
    }

    'init {
      val l = List.init(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil)))))))
      assert(List.length(l) == 5)
      assert(List.head(l) == 1)
    }

    'foldLeft {
      val sum = List.foldLeft(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil)))))), 0)(_ + _)
      assert(sum == 21)
    }

    'reverseFold {
      val l = List.reverse(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil)))))))
      assert(List.length(l) == 6)
      assert(List.head(l) == 6)
    }

    'foldRightViaLeft {
      val y = List.foldRight(Cons(2, Cons(3, Cons(5, Cons(10, Nil)))), 0)(_ - _)
      val x = List.foldRightViaLeft(Cons(2, Cons(3, Cons(5, Cons(10, Nil)))), 0)(_ - _)
      assert(x == y)
    }

    'appendViaFold {
      val l1 = Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
      val l2 = Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Nil)))))
      val l3 = List.append(l1, l2)
      assert(List.length(l3) == 10)
      assert(List.head(l3) == 0)
      assert(List.tail(l3) ==  Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Nil))))))))))
    }

    'flatten {
      val l1 = Cons(Cons(0, Cons(1, Nil)), Cons(Cons(2, Cons(3, Nil)), Nil))
      val l2 = List.flatten(l1)
      assert(List.length(l2) == 4)
      assert(List.head(l2) == 0)
      assert(List.tail(l2) == Cons(1, Cons(2, Cons(3, Nil))))
    }

    'map {
      val l1 = Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
      val l2 = List.map(l1)(_ + 1)
      assert(List.length(l2) == 5)
      assert(List.head(l2) == 1)
      assert(List.tail(l2) == Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    }

    'filter {
      val l1 = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Nil)))))))))
      val l2 = List.filter(l1)(_ % 2 == 0)
      assert(List.length(l2) == 4)
      assert(List.head(l2) == 2)
      assert(List.tail(l2) == Cons(4, Cons(6, Cons(8, Nil))))
    }

    'flatMap {
      val l1 = Cons(1, Cons(2, Cons(3, Nil)))
      val l2 = List.flatMap(l1)(i => List(i, i))
      assert(List.length(l2) == 6)
      assert(List.head(l2) == 1)
      assert(List.tail(l2) == Cons(1, Cons(2, Cons(2, Cons(3, Cons(3, Nil))))))
    }

    'filterViaFlatMap {
      val l1 = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Nil)))))))))
      val l2 = List.filterViaFlatMap(l1)(_ % 2 == 0)
      assert(List.length(l2) == 4)
      assert(List.head(l2) == 2)
      assert(List.tail(l2) == Cons(4, Cons(6, Cons(8, Nil))))
    }

    'hasSubsequence {
      val l1 = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Nil)))))))))
      val l2 = Cons(1, Cons(2, Nil))
      val l3 = Cons(3, Cons(4, Cons(5, Nil)))
      val l4 = Cons(6, Nil)
      val l5 = Cons(7, Cons(8, Cons(9, Nil)))
      val l6 = Cons(2, Cons(3, Cons(4, Cons(6, Nil))))
      val l7 = Cons(2, Cons(4, Cons(6, Nil)))
      val l8 = Cons(10, Nil)
      assert(List.hasSubsequence(l1, l2) == true)
      assert(List.hasSubsequence(l1, l3) == true)
      assert(List.hasSubsequence(l1, l4) == true)
      assert(List.hasSubsequence(l1, l5) == true)
      assert(List.hasSubsequence(l1, l6) == false)
      assert(List.hasSubsequence(l1, l7) == false)
      assert(List.hasSubsequence(l1, l8) == false)
      assert(List.hasSubsequence(Nil, l8) == false)
      assert(List.hasSubsequence(l1, Nil) == true)
    }
  }

}
