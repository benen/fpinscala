package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {

  /* Exercise 4.1 */
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => Some(a)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a) == true) => Some(a)
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /* Exercise 4.2 */
  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap { m => mean ( xs map (x => math.pow(x - m, 2)))}

  /* Exercise 4.3 */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(aa), Some(bb)) => Some(f(aa,bb))
    case _ => None
  }

  /* Exercise 4.4 */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight(Some(List.empty[A]): Option[List[A]])((opt, acc) => (acc, opt) match {
    case (Some(l), Some(a)) => Some(l.+:(a))
    case _ => None
  })

  /* Exercise 4.5 */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight(Some(List.empty[B]): Option[List[B]])((aa, acc) => (f(aa), acc) match {
    case (Some(b), Some(l)) => Some(l.+:(b))
    case _ => None
  })

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(a => a)
}