package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop0 { self =>
  def check: Boolean
  def &&(p: Prop0): Prop0 = new Prop0 {
    override def check: Boolean = self.check && p.check
  }
}

trait Prop1 { self =>
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop1): Prop1 =
    new Prop1 {
      override def check = self.check match {
        case Right(_) => p.check
        case left@Left(e) => left
      }
    }
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  /* Ex 8.9 i */
  def &&(p: Prop): Prop = Prop { (m, n, rng) =>
    val r1 = run(n, n, rng)
    if (r1.isFalsified)
      r1
    else {
      p.run(m, n, rng)
    }
  }

  /* Ex 8.9 ii */
  def ||(p: Prop): Prop = Prop { (m, n, rng) =>
    run(m, n, rng) match {
      case Falsified(f, s) => p.tag(f).run(m, n, rng)
      case success: Result => success
    }
  }

  def tag(m: String): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(f, s) => Falsified(s"$m\n$f", s)
      case success: Result => success
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
    successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  def forAll[A](g: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => randomStream(g)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"""
       |test case: $s
       |generated an exception: ${e.getMessage}
       |stack trace: ${e.getStackTrace.mkString("\n")}
    """.stripMargin
}

object ListProps {
  /* 8.14 i :Prop for List.sorted */
  lazy val intListGen: Gen[List[Int]] =
    //Gen.int.flatMap(i => Gen.listOf(Gen.int)(i)) // too slow
    Gen.choose(1, 1000).flatMap(Gen.listOf(Gen.int)(_))

  lazy val sortedProp: Prop =
      Prop.forAll(intListGen) { l: List[Int] =>
        l.sorted == l.reverse.sorted &&
        l.sorted.head == l.min &&
        l.sorted.last == l.max &&
        l.sorted.size == l.size
      }

  // Exercise 8.14: Prop for List.takeWhile
  lazy val takeWhileProp: Prop = {
    val f = (_: Int) <= 0
    val p1 = Prop.forAll(intListGen) { l: List[Int] =>
      l.takeWhile(f).forall(f) == true
    }
    val p2: Prop = ???
    p1 && p2
  }
}

object ParProps {
  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool) -> 0.25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    Prop.forAll(S ** g){ case s ** a  => f(a)(s).get }

  def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p1, p2)(_ == _)

  def checkPar[A](p: => Par[Boolean]): Prop = Prop { (_, _, _) =>
    val result = p(Executors.newCachedThreadPool).get
    if (result) Proved else Falsified("()", 0)
  }

  /* Ex 8.17 */
  lazy val fork: Prop = checkPar { Par.map2(Par.fork(Par.unit(1)), Par.unit(1))(_ == _) }
}

object Gen {

  /* Ex 8.4 */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen( State { rng0 =>
    val (rand, rng1) = rng0.nextInt
    val range = stopExclusive - start
    (Math.abs(rand % range) + start, rng1)
  })

  /* Ex 8.5 i */
  def unit[A](a: => A): Gen[A] = Gen(State{ rng0 =>
    (a, rng0)
  })

  /* Ex 8.5 ii */
  def boolean: Gen[Boolean] = Gen(State{ rng0 =>
    val (rand, rng1) = rng0.nextInt
    val bool = if (rand < 0) false else true
    (bool, rng1)
  })

  /* Ex 8.5 iii */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def int: Gen[Int] =
    Gen(State(RNG.int))

  def double: Gen[Double] =
    Gen(State(RNG.double))

  // here is an example on how to combine generators in a for-comprehension
  def option[A](gen: Gen[A]): Gen[Option[A]] =
    for {
        b <- Gen.boolean
        a <- gen
      } yield if (b) Some(a) else None

  def stringN(n: Int): Gen[String] = ???

  /* Ex 8.7 */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    for {
      s <- Gen.boolean
      r <- if (s) g1 else g2
    } yield r

  /* Ex 8.8 */
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val sum = g1._2 + g2._2
    for {
      s <- Gen.double
      gg1 <- g1._1
      gg2 <- g2._1
    } yield if ((s * sum) <= g1._2) gg1 else gg2
  }

  /* Ex 8.12 */
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen { n => Gen.listOfN(n, g)}

  /* Ex 8.13 */
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen { n => Gen.listOfN(n max 1, g) }

  /* Ex 8.16 */
  lazy val parInt: Gen[Par[Int]] = Gen.int.listOfN(Gen.choose(-100, 100))
    .map(
      _.map { n => Par.fork(Par.unit(n)) }
      .foldLeft(Par.fork(Par.unit(0))){ (acc, b) =>
        Par.fork(Par.map2(acc, b)(_ + _))
      }
    )
}

case class Gen[+A](sample: State[RNG,A]) {

  /* Ex 8.6 i */
  def map[B](f: A => B): Gen[B] = Gen(State{ rng0 =>
    val (a, rng1) = sample.run(rng0)
    (f(a), rng1)
  })

  /* Ex 8.6 ii */
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  /* Ex 8.6 iii */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))

  /* Ex 8.10 */
  def unsized: SGen[A] = SGen { (n) => this }
}

case class SGen[+A](forSize: Int => Gen[A]) {

  /* Ex 8.11 i */
  def apply(n: Int): Gen[A] = forSize(n)

  /* Ex 8.11 ii */
  def map[B](f: A => B): SGen[B] = SGen { n => forSize(n).map(f) }

  /* Ex 8.11 iii */
  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val fs: Int => Gen[B] = (n) => forSize(n).flatMap { (a) =>
      f(a).forSize(n)
    }
    SGen(fs)
  }

  /* Ex 8.11 iv */
  def **[B](s2: SGen[B]): SGen[(A,B)] = SGen { (n) => forSize(n) ** s2.forSize(n) }
}

object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}
