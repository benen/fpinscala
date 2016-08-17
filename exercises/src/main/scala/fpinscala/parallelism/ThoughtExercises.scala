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

  /* Exercise 7.8 */
  // Our fork won't work with Single Threadpool. Indeed, to make our laws hold given a fixed thread pool we need to
  // implement fork in such a way that all threads are guaranteed to complete... without exception!

  /* Exercise 7.9 */
  // Can we derive this from the free theorem law? In otherwords, if map(map(y)(f))(f) were to be applied using
  // f = lazyUnit(_), any combination of applications will be expected to yield the same result. So we can apply it
  // indefinitely. However, if we have a fixed threadpool of size n the nth application of lazy unit will then always
  // deadlock. 
}
