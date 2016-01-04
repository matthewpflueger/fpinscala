package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1, r) = rng.nextInt
    // book answer
    (if (i1 < 0) -(i1 + 1) else i1, r)
    // my answer to make double easier...
//    if (i1 == Integer.MIN_VALUE || i1 == Integer.MAX_VALUE) {
//      (Integer.MAX_VALUE - 1, r)
//    } else {
//      (Math.abs(i1), r)
//    }

    // my original answer
//    if (i1 == Integer.MIN_VALUE) {
//      (Integer.MAX_VALUE, r)
//    } else {
//      (Math.abs(i1), r)
//    }
  }

  val doubleViaMap = map(nonNegativeInt)(_/(Integer.MAX_VALUE.toDouble + 1))

  def double(rng: RNG): (Double, RNG) = {
    val (i1, r) = nonNegativeInt(rng)
    (i1/(Integer.MAX_VALUE.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1, r1) = nonNegativeInt(rng)
    val (d1, r2) = double(r1)
    ((i1, d1), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i1, d1), r) = intDouble(rng)
    ((d1, i1), r)
  }

//  def pipe2[A](f: RNG => (A, RNG))(rng: RNG): ()
//  def pipe[A](rng: RNG)(f: RNG => (A, RNG)): (A, RNG) = f(rng)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    //def &(f: RNG => (A, RNG))
    //double(rng) & double & double
//    pipe(rng)(double _)
    val (d1, r1) = RNG.double(rng)
    val (d2, r2) = RNG.double(r1)
    val (d3, r3) = RNG.double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(l: List[Int], r: RNG): (List[Int], RNG) =
      if (l.length >= count) (l, r)
      else {
        val (i, r2) = nonNegativeInt(r)
        loop(i :: l, r2)
      }
    loop(Nil, rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def sequence[A] = sequenceLeft[A] _

  def sequenceLeft[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    val (l, r) = fs.foldLeft(unit(List[A]()))((acc, f) => map2(f, acc)(_ :: _))(rng)
    (l.reverse, r)
  }

  def sequenceRight[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def sequenceMine[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      val (l, r) = fs.foldLeft((List.empty[A], rng)) { case ((l, r), rand) =>
        val (a1, r2) = rand(r)
        (a1 :: l, r2)
      }
      (l.reverse, r)
    }

  def intsViaSequence(count: Int) = sequence(List.fill(count)(nonNegativeInt _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def nonNegativeLessThan(lessThan: Int): Rand[Int] =
    flatMap(nonNegativeInt _)(i => if (i >= lessThan) unit(i) else nonNegativeLessThan(lessThan))
    // book answer
    //flatMap(nonNegativeInt) { i =>
      //val mod = i % n
      //if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n) }


  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State { s => (f(a), s) })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => State { s => val (b, s1) = sb.run(s); (f(a, b), s1) })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s1 => val (a, s2) = run(s1); f(a).run(s2) }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { mach =>
    val m1 = inputs.foldLeft(mach) {
      case (m, Coin) if m.locked && m.candies > 0 => m.copy(locked = false, coins = m.coins + 1)
      case (m, Turn) if !m.locked => m.copy(locked = true, candies = m.candies - 1)
      case (m, _) => m
    }
    ((m1.coins, m1.candies), m1)
  }

  def unit[S, A](a: A): State[S, A] = State { (a, _) }
}
