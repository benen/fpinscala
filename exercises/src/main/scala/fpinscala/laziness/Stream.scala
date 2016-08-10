package fpinscala.laziness

import Stream._
trait Stream[+A] {

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  /* Ex 5.1 */
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => Nil
  }

  /* Ex 5.2 i */
  def take(n: Int): Stream[A] = this match {
    case Empty if n > 0 => throw new NoSuchElementException("Stream is empty")
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  /* Ex 5.2 ii */
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /* Ex 5.3 */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  /* Ex 5.4 */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /* Ex 5.5 */
  def takeWhileViaFold(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  /* Ex 5.6 */
  def headOption: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  /* Ex 5.7 i */
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

  /* Ex 5.7 ii */
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)

  /* Ex 5.7 iii */
  def append[B >: A](that: => Stream[B]): Stream[B] =
    foldRight(that: Stream[B])((a, b) => cons(a, b))

  /* Ex 5.7 iv */
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = sys.error("todo")

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = sys.error("todo")

  def tails: Stream[Stream[A]] = sys.error("todo")

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = sys.error("todo")

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")

  def constant[A](a: A): Stream[A] = sys.error("todo")

  lazy val fibs: Stream[Int] = sys.error("todo")
}