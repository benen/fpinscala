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

case class Prop(run: (TestCases,RNG) => Result) {
  /* Ex 8.9 i */
  def &&(p: Prop): Prop = Prop { (n, rng) =>
    val r1 = run(n, rng)
    if (r1.isFalsified)
      r1
    else {
      p.run(n, rng)
    }
  }

  /* Ex 8.9 ii */
  def ||(p: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Passed => Passed
      case Falsified(f, s) => p.tag(f).run(n, rng)
    }
  }

  def tag(m: String): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Passed => Passed
      case Falsified(f, s) => Falsified(s"$m\n$f", s)
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

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

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object ListProps {
  // Exercise 8.14: Prop for List.sorted
  lazy val intListGen: Gen[List[Int]] = ???
  lazy val sortedProp: Prop =
      Prop.forAll(intListGen) { l: List[Int] =>
        ???
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

  def listOf[A](g: Gen[A]): SGen[List[A]] = ???

  def listOf1[A](g: Gen[A]): SGen[List[A]] = ???

  lazy val parInt: Gen[Par[Int]] = ???
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


  def listOf: SGen[List[A]] = Gen.listOf(this)

  def listOf1: SGen[List[A]] = Gen.listOf1(this)

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))

  /* Ex 8.10 */
  def unsized: SGen[A] = SGen { (n) => this }
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = ???

  def map[B](f: A => B): SGen[B] = ???

  def flatMap[B](f: A => SGen[B]): SGen[B] = ???

  def **[B](s2: SGen[B]): SGen[(A,B)] = ???
}
