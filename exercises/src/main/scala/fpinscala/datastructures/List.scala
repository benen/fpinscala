package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /* Exercise 3.2 */
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => xs
    case Nil => throw new RuntimeException("List is empty")
  }

  /* Exercise 3.3 */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(x, xs) => Cons(h, xs)
    case Nil => throw new RuntimeException("List is empty")
  }

  /* Exercise 3.4 */
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(x, xs) if n > 0 => drop(xs, n - 1)
    case Cons(x, xs) => Cons(x, xs)
    case Nil => Nil
  }

  /* Exercise 3.5 */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case Cons(x, xs) => Cons(x, xs)
    case Nil => Nil
  }

  /* Exercise 3.6 */
  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case Nil => throw new RuntimeException("List is empty")
  }

  /* Exercise 3.7 */
  // No it cannot, as it must traverse to the end of the List first before it can evaluate it's function f

  /* Exercise 3.8 */
  // It states they are recursive.

  /* Exercise 3.9 */
  def length[A](l: List[A]): Int = foldRight(l, 0)((x, acc) => acc + 1)

  /* Exercise 3.10 */
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /* Exercise 3.11 */
  def sumViaFoldLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def productViaFoldLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((acc, x) => acc + 1)

  /* Exercise 3.12 */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A]){(acc, x) => Cons(x, acc)}

  /* Exercise 3.13 */
  // Builds up a chain of functions, not stack safe
  def foldLeftViaRight[A, B](l: List[A], z: B)(f: (B, A) => B) = {
    foldRight(l, (b: B) => b){(x, acc) =>
      (b: B) => acc(f(b, x))
    }(z)
  }
  // Tail recursive but requiring of two passes
  def foldRightViaLeft[A, B](l: List[A], z: B)(f: (A, B) => B) = foldLeft(reverse(l), z)((acc, x) => f(x, acc))

  /* Exercise 3.14 */
  // appends the last element of a1 with a2, then unwinds the stack prepending the elements of a1 in reverse
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((x, acc) => Cons(x, acc))
  // simple solution using reverse of the first list to replicate behaviour above
  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((acc, x) => Cons(x, acc))

  /* Exercise 3.15 */
  // reverse the list then append via fold left. will require way too many passes.
  def concat[A](ll: List[List[A]]): List[A] = foldLeft(reverse(ll), Nil: List[A])((acc, x) => appendViaFoldLeft(x, acc))

  /* Exercise 3.16 */
  def addOne(l: List[Int]): List[Int] = foldRightViaLeft(l, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  /* Exercise 3.17 */
  def doubleToString(l: List[Double]): List[String] = foldRightViaLeft(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  /* Exercise 3.18 */
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRightViaLeft(l, Nil:List[B])((x, acc) => Cons(f(x), acc))

  /* Exercise 3.19 */
  def filter[A](l: List[A])(f: A => Boolean) = foldRightViaLeft(l, Nil:List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

  /* Exercise 3.20 */
  def flatMap[A, B](l: List[A])(f: A => List[B]) = concat(map(l)(f))

  /* Exercise 3.21 */
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean) = flatMap(l)(x => if (f(x)) List(x) else Nil)

  /* Exercise 3.22 */
  def addPairwise(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, addPairwise(xs1, xs2))
    case (Nil, _) => Nil
    case (_, Nil) => Nil
  }

  /* Exercise 3.23 */
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
    case _ => Nil
  }

  /* Exercise 3.24 */
  def take[A](l: List[A], n: Int): List[A] = {
    def go(ls: List[A], nn: Int, acc: List[A]): List[A] = ls match {
      case Cons(x, xs) if nn > 0 => go(xs, nn - 1, Cons(x, acc))
      case _ => reverse(acc)
    }
    go(l, n, Nil)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val subLength = length(sub)
    def go(p: List[A]): Boolean = p match {
      case Cons(x, xs) if take(p, subLength) == sub => true
      case Cons(x, xs) => go(xs)
      case Nil => false
    }
    go(sup)
  }
}
