package fpinscala.datastructures

import utest._

object TreeTest extends TestSuite {

  val tests = TestSuite {
    'size {
      assert(Tree.size(Leaf(1)) == 1)
      assert(Tree.size(Branch(Leaf(1), Leaf(1))) == 2)
      assert(Tree.size(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Branch(Leaf(1), Leaf(1)))) == 5)
    }
    'maximum {
      assert(Tree.maximum(Branch(Branch(Branch(Leaf(10), Leaf(9)), Leaf(8)), Branch(Leaf(7), Leaf(6)))) == 10)
      assert(Tree.maximum(Branch(Leaf(7), Leaf(6))) == 7)
      assert(Tree.maximum(Leaf(6)) == 6)
    }
    'depth {
      assert(Tree.depth(Leaf(0)) == 1)
      assert(Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(2)))) == 3)
      assert(Tree.depth(Branch(Branch(Branch(Leaf(10), Leaf(9)), Leaf(8)), Branch(Leaf(7), Leaf(6)))) == 4)
      assert(Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Branch(Leaf(4), Branch(Leaf(5), Leaf(5))))))) == 6)
    }
    'map {
      assert(Tree.map(Leaf(0))(_ + 1) == Leaf(1))
      assert(Tree.map(Branch(Leaf(2), Branch(Leaf(2), Leaf(2))))(_ + 3) == Branch(Leaf(5), Branch(Leaf(5), Leaf(5))))
    }
  }

}
