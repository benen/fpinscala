package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  /* Exercise 3.25 */
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  /* Exercise 3.26 */
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  /* Exercise 3.27 */
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  /* Exercise 3.28 */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  /* Exercise 3.29 */
  def fold[A, B](tree: Tree[A])(l: A => B)(b: (B, B) => B): B = tree match {
    case Leaf(value) => l(value)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }
  def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)
  def maximumViaFold(tree: Tree[Int]): Int = fold(tree)(x => x)(_ max _)
  def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(a => 0)((b1, b2) => 1 + (b1 max b2))
  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree) { (a) => Leaf(f(a)): Tree[B] } { (b1, b2) => Branch(b1, b2) }
}
