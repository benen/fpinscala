package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps

import language.higherKinds
import scala.collection.Set

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  /* Ex 10.1 i */
  lazy val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  /* Ex 10.1 ii */
  lazy val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  /* Ex 10.1 iii */
  lazy val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 | a2
    override def zero: Boolean = false
  }

  /* Ex 10.1 iv */
  lazy val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 & a2
    override def zero: Boolean = true
  }

  /* Ex 10.2 */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  /* Ex 10.3 */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 andThen a2
    override def zero: (A) => A = a => a
  }

  import fpinscala.testing._
  import Prop._

  /* Ex 10.4 */
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen ** gen ** gen) { case ((a, b), c) =>
      if (m.op(a, m.op(b, c)) == m.op(m.op(a, b), c))
        true
      else {
        println(
          s"""
          |Associative law: ${m.op(a, m.op(b, c))} did not equal ${m.op(m.op(a, b), c)}
          |  values:
          |    a=$a
          |    b=$b
          |    c=$c
          |""".stripMargin
        )
        false
      }
    } &&
    forAll(gen){ (a) =>
      if (m.op(a, m.zero) == a)
        true
      else {
        println(s"Identity right: ${m.op(a, m.zero)} did not equal $a")
        false
      }
    } &&
    forAll(gen) { (a) =>
      if (m.op(m.zero, a) == a)
        true
      else {
        println(s"Identity left: ${m.op(m.zero, a)} did not equal $a")
        false
      }
    }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  /* Ex 10.5 */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a))) // switching to stack safe

  /* Ex 10.6 i */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, new Monoid[B => B] {
      override def op(a1: (B) => B, a2: (B) => B): (B) => B = endoMonoid[B].op(a2, a1)
      override def zero: (B) => B = endoMonoid[B].zero
    } )(f.curried)(z)

  /* Ex 10.6 ii */
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B]){ a => b => f(b, a) }(z)

  /* Ex 10.7 */
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as match {
    case Seq() => m.zero
    case Seq(a1) => f(a1)
    case x =>
      val (s1, s2) = x.splitAt(x.size / 2)
      m.op(foldMapV(s1, m)(f), foldMapV(s2, m)(f))
  }

  /* Ex 10.8 i */
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]{
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    override def zero: Par[A] = Par.lazyUnit(m.zero)
  }

  /* Ex 10.8 ii */
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(a => Par.lazyUnit(f(a)))

  /*Ex 10.9 */
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    case class Acc(min: Int, max: Int, ordered: Boolean)
    foldMapV(ints, new Monoid[Acc] {
      def op(a1: Acc, a2: Acc): Acc = Acc(a1.min, a2.max, a1.ordered && a2.ordered && a1.max <= a2.min)
      def zero: Acc = Acc(Int.MinValue, Int.MaxValue, true)
    })(i => Acc(i, i, true)).ordered
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  /* Ex 10.10 */
  lazy val wcMonoid: Monoid[WC] = new Monoid[WC]{
    override def op(a1: WC, a2: WC): WC = {
      (a1, a2) match {
      case (Stub(s1), Stub(s2))                 => Stub(s1 + s2)
      case (Stub(s1), Part(s2, c, s3))          => Part(s1 + s2, c, s3)
      case (Part(s1, c, s2),  Stub(s3))         => Part(s1, c, s2 + s3)
      case (Part(s1, c1, s2), Part(s3, c2, s4)) => Part(s1, c1 + c2 + (if ((s2 + s3).nonEmpty) 1 else 0), s4)
    }}
    override def zero: WC = Stub("")
  }

  /* Ex 10.11 */
  def countWords(s: String): Int = {
    foldMapV(s.toCharArray, wcMonoid)(_ match {
      case c if c.toString.matches("\\s") => Part("", 0, "")
      case c => Stub(c.toString)
    }) match {
      case Stub(s) if s.isEmpty => 0
      case Stub(s) if s.nonEmpty => 1
      case Part(start, c, end) => c + (if (start.nonEmpty) 1 else 0) + (if (end.nonEmpty) 1 else 0)
    }
  }

  /* Ex 10.16 */
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  /* Ex 10.17 */
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  /* Ex 10.18 */
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid.endoMonoid

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => f.curried(_: B)(a))(endoMonoid[B])(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, acc) => mb.op(f(a), acc))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldLeft(as)(List.empty[A])((acc, a) => acc :+ a)
}

/* Ex 10.12 i */
object ListFoldable extends Foldable[List] {

  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMap(as, mb)(f)
}

/* Ex 10.12 ii */
object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)
}

/* Ex 10.12 iii */
object StreamFoldable extends Foldable[Stream] {

  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

/* Ex 10.13 */
object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    foldMap(as)(a => f.curried(_: B)(a))(Monoid.endoMonoid[B])(z)

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    foldMap(as)(f.curried)(Monoid.endoMonoid[B])(z)
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

