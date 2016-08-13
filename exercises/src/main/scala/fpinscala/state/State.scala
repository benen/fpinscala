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

  /* Ex 6.1 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i, r) if i < 0 => (-(i + 1), r)
    case (i, r) => (i, r)
  }

  /* Ex 6.2 */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble + 1.0), r)
  }

  /* Ex 6.3 i */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r0) = rng.nextInt
    val (d, r1) = double(r0)
    ((i, d), r1)
  }

  /* Ex 6.3 ii */
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  /* Ex 6.3 iii */
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /* Ex 6.4 */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(acc: List[Int], n: Int, r: RNG): (List[Int], RNG) = {
      val (i, r1) = r.nextInt
      if (n > 1)
        go(acc :+ i, n - 1, r1)
      else
        (acc :+ i, r1)
    }
    if (count > 0) go(List(), count, rng) else (List(), rng)
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

  /* Ex 6.5 */
  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1.0))

  /* Ex 6.6 */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { (rng) =>
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  /* Ex 6.7 i */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      fs.foldLeft((List[A](), rng)){ (acc, ra) =>
        val (a, rn) = ra(acc._2)
        (acc._1 :+ a, rn)
      }

  /* Ex 6.7 ii */
  def intsViaSequence(count: Int): Rand[List[Int]] =
      sequence(List.fill(count)(int))

  /* Ex 6.8 i */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  /* Ex 6.8 ii */
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){ i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, _)
    else
      nonNegativeLessThan(n)
  }

  /* Ex 6.9 i */
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => (f(a), _))

  /* Ex 6.9 ii */
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a =>
    flatMap(rb)( b => (f(a, b), _))
  )
}

case class State[S,+A](run: S => (A, S)) {
  /* Ex 6.10 ii */
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s1) = run(s)
    (f(a), s1)
  })

  /* Ex 6.10 iii */
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, s1) = run(s)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  })

  /* Ex 6.10 iv */
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  /* Ex 6.10 i */
  def unit[S, A](a: A): State[S, A] = State((a, _))

  /* Ex 6.10 v */
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = State(s => {
    sas.foldLeft((List[A](), s)){ (acc, sa) =>
      val (a, s1) = sa.run(acc._2)
      (acc._1 :+ a, s1)
    }
  })

  /* Ex 6.11 */
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { machine => {
    val result = inputs.foldLeft(machine)((m, i) => i match {
      case Coin => if (m.locked && m.candies > 0) m.copy(locked = false, coins = m.coins + 1) else m
      case Turn => if (!m.locked && m.candies > 0) m.copy(locked = true, candies = m.candies - 1) else m
    })
    ((result.coins, result.candies), result)
  }}
}