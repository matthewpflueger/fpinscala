package fpinscala.laziness

import utest._

object StreamTest extends TestSuite {
  def mkStream = Stream((1 to 10).toArray: _*)

  val tests = TestSuite {
    'toList {
      assert(mkStream.toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    }

    'take {
      assert(mkStream.take(5).toList == List(1, 2, 3, 4, 5))
      assert(mkStream.take(1).toList == List(1))
      assert(mkStream.take(0).toList == List())
      assert(mkStream.take(-1).toList == List())
    }

    'drop {
      assert(mkStream.drop(5).toList == List(6, 7, 8, 9, 10))
      assert(mkStream.drop(1).toList == List(2, 3, 4, 5, 6, 7, 8, 9, 10))
      assert(mkStream.drop(0).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
      assert(mkStream.drop(-1).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    }

    'takeWhile {
      assert(mkStream.takeWhile(_ < 5).toList == List(1, 2, 3, 4))
    }

    'forAll {
      assert(!mkStream.forAll(_ < 5))
      assert(mkStream.forAll(_ < 20))
    }

    'takeWhileFoldRight {
      assert(mkStream.takeWhileFoldRight(_ < 5).toList == List(1, 2, 3, 4))
    }

    'headOption {
      assert(mkStream.headOption.contains(1))
      assert(Stream.empty[Int].headOption.isEmpty)
    }

    'map {
      assert(mkStream.map[Int](_ + 2).toList == List(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    }

    'filter {
      assert(mkStream.filter(_ % 2 == 0).toList == List(2, 4, 6, 8, 10))
      assert(mkStream.filter(_ % 2 != 0).toList == List(1, 3, 5, 7, 9))
    }

    'append {
      assert(mkStream.append(Stream(11, 12, 13)).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
    }

    'flatMap {
      assert(mkStream.flatMap[Int](i => Stream(i + 10)).toList == List(11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
    }

    'constant {
      assert(Stream.constant(5).take(10).toList == List(5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
      assert(Stream.constantEfficient(5).take(10).toList == List(5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
    }

    'from {
      assert(Stream.from(5).take(10).toList == List(5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
    }

    'fibs {
      assert(Stream.fibs.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
    }

    'unfold {
      assert(Stream.unfold(0)(s => if (s < 10) Some((s, s + 1)) else None).toList == List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    'asUnfold1 {
      assert(Stream.fibsUnfold.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
      assert(Stream.fromUnfold(5).take(10).toList == List(5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
      assert(Stream.constantUnfold(5).take(10).toList == List(5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
      assert(Stream.onesUnfold.take(10).toList == List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    }

    'asUnfold2 {
      assert(Stream
          .constant(5)
          .take(3)
          .zipWith(Stream.constant(5).take(3))(_ + _)
          .toList == List(10, 10, 10))
      assert(Stream
          .constant(3)
          .take(3)
          .zipWithUnfold(Stream.constant(3).take(3))(_ + _)
          .toList == List(6, 6, 6))

      assert(Stream
          .constant(3)
          .take(3)
          .zipAll(Stream.constant(4).take(4))
          .toList == List(
              (Some(3), Some(4)),
              (Some(3), Some(4)),
              (Some(3), Some(4)),
              (None, Some(4))))

      assert(mkStream.mapUnfold[Int](_ + 2).toList == List(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

      assert(mkStream.takeUnfold(5).toList == List(1, 2, 3, 4, 5))
      assert(mkStream.takeUnfold(1).toList == List(1))
      assert(mkStream.takeUnfold(0).toList == List())
      assert(mkStream.takeUnfold(-1).toList == List())

      assert(mkStream.takeWhileUnfold(_ < 5).toList == List(1, 2, 3, 4))
    }

    'startsWith {
      assert(Stream(1, 2, 3).startsWith(Stream(1, 2)))
      assert(!Stream(1, 2, 3).startsWith(Stream(3)))
      assert(!Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)))
    }
  }

}
