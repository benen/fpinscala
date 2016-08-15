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
  }


}
