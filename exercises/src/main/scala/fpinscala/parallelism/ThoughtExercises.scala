package fpinscala.parallelism

import java.util.concurrent.{Callable, Executors, Future}

object ThoughtExercises {

  val pool = Executors.newFixedThreadPool(5)

  /* Exercise 7.2 */
  case class Par[A](a: Callable[A])

  object Par {
    def unit[A](a: A): Par[A] = Par(new Callable[A]{ override def call: A = a})
    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???
    def fork[A](a: => Par[A]): Par[A] = ???
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    def run[A](a: Par[A]): A = pool.submit(a.a).get()
    def map[A, B](pa: Par[A])(f: A => B): Par[B] = ???
  }

  /* Exercise 7.7 */
  import Par._
  val y = unit(1)
  val y1 = 0
  def f: Int => Int = _ + 1
  def g: Int => Int = _ + 10
  def id: Int => Int = _
  map(y)(id) == y
  map(map(y)(id))(id) == map(y)(id)
  map(y)(f) == f(y1) // look at return types
  map(map(y)(g))(f) == g(f(y1))
  map(map(y)(g))(f) == (g compose f)(y1)
  map(map(y)(g))(f) ==  map(y)(g compose f)
  // ???


}
