package fpinscala.state

import fpinscala.state.RNG.Simple
import utest.TestSuite
import utest._

object StateTest extends TestSuite {
  val minRNG = new RNG { override def nextInt: (Int, RNG) = (Integer.MIN_VALUE, this) }
  val maxRNG = new RNG { override def nextInt: (Int, RNG) = (Integer.MAX_VALUE, this) }
  val simpleRNG = new Simple(42)

  def assertDouble(d: Double): Unit = {
    assert(d < 1)
    assert(d >= 0)
  }

  def assertInt(i: Int): Unit = assert(i >= 0)

  val tests = TestSuite {
    'nonNegativeInt {
      val (i1, _) = RNG.nonNegativeInt(minRNG)
      assertInt(i1)
      val (i2, _) = RNG.nonNegativeInt(simpleRNG)
      assertInt(i2)
    }

    'double {
      val (d2, _) = RNG.double(simpleRNG)
      assertDouble(d2)

      val (d3, _) = RNG.double(minRNG)
      assertDouble(d3)

      val (d4, _) = RNG.double(maxRNG)
      assertDouble(d4)
    }

    'doubleTuples {
      val ((d1, i1), r1) = RNG.doubleInt(simpleRNG)
      assertDouble(d1)
      assertInt(i1)

      val ((i2, d2), r2) = RNG.intDouble(r1)
      assertDouble(d1)
      assertInt(i1)
      assert(d2 != d1)
      assert(i1 != i2)

      val ((d3, d4, d5), _) = RNG.double3(r2)
      assertDouble(d3)
      assertDouble(d4)
      assertDouble(d5)
      assert(d3 != d4)
      assert(d3 != d5)
      assert(d4 != d5)
      assert(d1 != d3)
      assert(d1 != d4)
      assert(d1 != d5)
      assert(d2 != d3)
      assert(d2 != d4)
      assert(d2 != d5)
    }

    'ints {
      val (ints, r) = RNG.ints(3)(simpleRNG)
      assert(ints.length == 3)
      assert(ints(0) != ints(1))
      assert(ints(0) != ints(2))
      assert(ints(1) != ints(2))
    }

    'doubleViaMap {
      val (d1, _) = RNG.doubleViaMap(simpleRNG)
      assertDouble(d1)
    }

    'map2 {
      val (i, _) = RNG.map2(RNG.nonNegativeInt, RNG.nonNegativeInt)((a, b) => a + b)(simpleRNG)
      val (i2, r1) = RNG.nonNegativeInt(simpleRNG)
      val (i3, _) = RNG.nonNegativeInt(r1)
      assert(i == i2 + i3)
    }

    'intsViaSequence {
      val (ints, r) = RNG.intsViaSequence(3)(simpleRNG)
      assert(ints.length == 3)
      assert(ints(0) != ints(1))
      assert(ints(0) != ints(2))
      assert(ints(1) != ints(2))
    }

    'mapViaFlatMap {
      val (i, _) = RNG.mapViaFlatMap(RNG.nonNegativeInt _)(_ * 2)(simpleRNG)
      val (i2, _) = RNG.nonNegativeInt(simpleRNG)
      assert(i == i2 * 2)
    }

    'map2ViaFlatMap {
      val (i, _) = RNG.map2ViaFlatMap(RNG.nonNegativeInt, RNG.nonNegativeInt)((a, b) => a + b)(simpleRNG)
      val (i2, r1) = RNG.nonNegativeInt(simpleRNG)
      val (i3, _) = RNG.nonNegativeInt(r1)
      assert(i == i2 + i3)
    }

    'mapState {
      val s = State[Int, Int](s => (s + 10, s))
      val (a, s2) = s.map[Int](_ + 10).run(0)
      assert(a == 20)
      assert(s2 == 0)
    }

    'map2State {
      val s = State[Int, Int](s => (s + 10, s))
      val (a, s2) = s.map2[Int, Int](State[Int, Int](s => (s + 20, s)))((a, b) => a + b).run(0)
      assert(a == 30)
      assert(s2 == 0)
    }

    'flatMapState {
      val s = State[Int, Int](s => (s + 10, s))
      val (a, s2) = s.flatMap(a => State { s => (a + s, s) }).run(0)
      assert(a == 10)
      assert(s2 == 0)
    }

    'simulateMachine {
      val ((coins, candy), s1) = State.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 5, 10))
      assert(coins == 14)
      assert(candy == 1)

      val ((coins1, candy1), s2) = State.simulateMachine(List(Turn, Coin, Coin, Turn, Turn, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Turn, Turn)).run(Machine(true, 5, 10))
      assert(coins1 == 14)
      assert(candy1 == 1)
    }
  }
}
