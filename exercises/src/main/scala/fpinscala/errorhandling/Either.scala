package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {

  /* Exercise 4.6 i */
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case left: Left[E] => left
  }

  /* Exercise 4.6 ii */
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case left: Left[E] => left
  }

  /* Exercise 4.6 iii */
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case right: Right[B] => right
    case left: Left[E] => b
  }

  /* Exercise 4.6 iv */
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(a), Right(b)) => Right(f(a, b))
    case (Right(a), b: Left[EE]) => b
    case (a: Left[EE], _) => a
  }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {

  /* Exercise 4.7 i */
  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
    def go(acc: List[A], xs: List[Either[E, A]]): Either[E, List[A]] = xs match {
      case Nil => Right(acc)
      case Left(e) :: _ => Left(e)
      case Right(a) :: rs => go(acc :+ a, rs)
    }
    go(List[A](), es)
  }

  /* Exercise 4.7 ii */
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    def go(acc: List[B], xs: List[A]): Either[E, List[B]] = xs match {
      case Nil => Right(acc)
      case x :: xs => f(x) match {
        case Right(b) => go(acc :+ b, xs)
        case left: Left[E] => left
      }
    }
    go(List[B](), es)
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}