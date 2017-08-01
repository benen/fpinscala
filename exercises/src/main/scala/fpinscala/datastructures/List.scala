package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /* Ex. 3.2 */
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => throw new IllegalArgumentException("List cannot be empty")
  }

  /* Ex. 3.3 */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case Nil => throw new IllegalArgumentException("List cannot be empty")
  }

  /* Ex. 3.4 */
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case c => c
  }

  /* Ex. 3.5 */
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case c => c
  }

  /* Ex. 3.6 */
  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, t) if t == Nil => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => throw new IllegalArgumentException("List cannot be empty")
  }

  /* Ex. 3.9 */
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, sum) => sum + 1)

  /* Ex. 3.10 */
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
    case Nil => z
  }

  /* Ex. 3.11 */
  def sumLeft(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def productLeft(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  /* Ex. 3.12 */
  def reverse[A](l: List[A]) = foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))

  /* Ex. 3.13 */
  def foldLeftViaRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, (b: B) => b){ (a, acc) => (b: B) => acc(f(b, a)) }(z)
  }
  def foldRightViaLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((b, a) => f(a, b))
  }

  /* Ex. 3.14 */
  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2){(a, acc) => Cons(a, acc)}

  /* Ex. 3.15 */
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A]){ (a, acc) => List.append(a, acc)}

  /* Ex. 3.16 */
  def addOne(l: List[Int]): List[Int] =
    List.reverse(foldLeft(l, Nil: List[Int]){ (acc, a) => Cons(a + 1, acc) })

  /* Ex. 3.17 */
  def asString(l: List[Double]): List[String] =
    List.reverse(foldLeft(l, Nil: List[String]){ (acc, a) => Cons(a.toString, acc) })

  /* Ex. 3.18 */
  def map[A,B](l: List[A])(f: A => B): List[B] =
    List.reverse(foldLeft(l, Nil: List[B]){(acc, a) => Cons(f(a), acc)})

  /* Ex. 3.19 */
  def filter[A](l: List[A], p: A => Boolean) =
    List.reverse(foldLeft(l, Nil: List[A]){(acc, a) => if (p(a)) Cons(a, acc) else acc })

  /* Ex. 3.20 */
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRightViaLeft(l, Nil: List[B])((a, acc) => List.append(f(a), acc))

  /* Ex. 3.21 */
  def filterViaFlatmap[A](l: List[A], p: A => Boolean): List[A] = {
    flatMap(l)(a => if (p(a)) List(a) else Nil)
  }

  /* Ex. 3.22 */
  def addTwo(l1: List[Int], l2: List[Int]): List[Int] = {
    @tailrec
    def go(i1: List[Int], i2: List[Int], acc: List[Int] = Nil): List[Int] = (i1, i2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => go(t1, t2, Cons(h1 + h2, acc))
      case _ => acc
    }
    reverse(go(l1, l2))
  }

  /* Ex. 3.23 */
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    @tailrec
    def go(i1: List[A], i2: List[B], acc: List[C] = Nil): List[C] = (i1, i2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => go(t1, t2, Cons(f(h1, h2), acc))
      case _ => acc
    }
    reverse(go(l1, l2))
  }

  /* Ex. 3.24 */
  @tailrec
  def startsWith[A](l1: List[A], l2: List[A]): Boolean = (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => startsWith(t1, t2)
    case (_, Nil) => true
    case (_, _) => false
  }

  @tailrec
  def hasSubsequence[A](l1: List[A], l2: List[A]): Boolean = {
    startsWith(l1, l2) || hasSubsequence(List.tail(l1), l2)
  }

}
