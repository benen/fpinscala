package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  /* Ex. 3.25 */
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  /* Ex. 3.26 */
  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(n) => n
    case Branch(l, r) => max(l) max max(r)
  }

  /* Ex. 3.27 */
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => depth(l) max depth(r) + 1
  }

  /* Ex. 3.28 */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /* Ex. 3.29 */
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)(_ + _ + 1)
  }

  def maxViaFold(tree: Tree[Int]): Int = {
    fold(tree)(x => x)(_ max _)
  }

  def depthViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)(_ max _ + 1)
  }

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(a => Leaf(f(a)): Tree[B])((b1, b2) => Branch(b1, b2))
  }
}