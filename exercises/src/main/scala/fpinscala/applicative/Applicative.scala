package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] { self =>

  implicit class ApplicativeOps[A](fa: F[A]) {
    def apply[B](fab: F[A => B]): F[B] = self.apply(fab)(fa)
    def map[B](f: A => B): F[B] = self.map(fa)(f)
  }

  def unit[A](a: => A): F[A]

  /* Ex 12.1 i */
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldLeft(unit(List.empty[A])){ (acc, a) => map2(acc, a)(_ :+ _) }

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldLeft(unit(List.empty[B])){ (acc, a) => map2(acc, f(a))(_ :+ _) }

  /* Ex 12.1 ii */
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  /* Ex 12.1 iii */
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((_,_))

  /* Ex 12.2 i */
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fa, fab)((a, ab) => ab(a))

  /* Ex 12.2 ii */
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  /* Ex 12.2 iii */
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  /* Ex 12.3 i */
  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val fbcd: F[B => C => D] = apply(unit(f.curried))(fa)
    val fcd: F[C => D] = apply(fbcd)(fb)
    apply(fcd)(fc)
  }

  /* Ex 12.3 ii */
  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val fbcde: F[B => C => D => E] = apply(unit(f.curried))(fa)
    val fcde: F[C => D => E] = apply(fbcde)(fb)
    val fde: F[D => E] = apply(fcde)(fc)
    apply(fde)(fd)
  }

  def factor[A,B](fa: F[A], fb: F[A]): F[(A,B)] = ???

  /* Ex 12.8 */
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) =
        (self.unit(a), G.unit(a))
      override def apply[A, B](fab: (F[(A) => B], G[(A) => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }
  }

  /* Ex 12.9 */
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] =
        self.map(self.unit(a))(aa => G.unit(aa))
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
    }

  /* Ex 12.12 */
  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
  ofa.foldLeft(unit(Map.empty[K, V])){ case (acc, (k, fv)) =>
    self.map2(acc, fv)((mkv, v) => mkv + (k -> v))
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] { self =>
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  /* Ex 12.11 */
//  def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = new Monad[({type f[x] = F[G[x]]})#f] {
//    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
//    override def flatMap[A, B](ma: F[G[A]])(f: (A) => F[G[B]]): F[G[B]] = self.flatMap(ma)(ga => G.flatMap(ga)(a => f(a)))
//  }

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  /* Ex 12.5 */
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = ma match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  lazy val listApplicative: Applicative[List] = new Applicative[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] = fa zip fb map f.tupled
  }

  lazy val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = for {
      a <- fa
      b <- fb
    } yield (f(a, b))
  }

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  /* Ex 12.6 */
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
          case (Success(a), Failure(h, t)) => Failure(h, t)
          case (Failure(h, t), Success(b)) => Failure(h, t)
        }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  /* Ex 12.14 */
  type Id[A] = A
  val idApplicative = new Applicative[Id] {
    override def unit[A](a: => A): Id[A] = a
    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] = {
    traverse[Id, A, B](fa)(f)(idApplicative)
  }

  import Applicative._

  // Ex 12.15
  // Can you define map in terms of foldMap?
  // You need a constructor that allows us to 'fuse' or codistribute multiple values

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  /* Ex 12.16 */
  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, a) => (a.head, a.tail))._1

  /* Ex 12.17 */
  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, s) => (s, f(s, a)))._2

  /* Ex 12.18 */
  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  /* Ex 12.19 */
  def compose[Z[_]](implicit Z: Traverse[Z]): Traverse[({type f[x] = F[Z[x]]})#f] =
    new Traverse[({type f[x] = F[Z[x]]})#f] {
      override def traverse[G[_]: Applicative, A, B](fa: F[Z[A]])(f: (A) => G[B]): G[F[Z[B]]] =
        self.traverse(fa)((za: Z[A]) => Z.traverse(za)(f))
    }
}

object Traverse {
  /* Ex 12.13 i */
  lazy val listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldLeft(G.unit(List.empty[B]))((acc, a) => G.map2(acc, f(a))(_ :+ _))
  }

  /* Ex 12.13 ii */
  lazy val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case Some(a) => G.map(f(a))(a => Option(a))
        case None => G.unit(None)
      }
  }

  /* Ex 12.13 iii */
  lazy val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(fa.head), listTraverse.traverse(fa.tail)(traverse(_)(f)))(Tree(_, _))
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
