package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  /* Ex. 7.3 */
  def map2WithTimeouts[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => new Future[C] {
      override def cancel(b: Boolean): Boolean = false
      override def isCancelled: Boolean = false
      override def isDone: Boolean = false
      override def get(): C = get(Long.MaxValue, TimeUnit.DAYS)
      override def get(l: Long, timeUnit: TimeUnit): C = {

        // This doesn't actually respect timeouts during the application of f. A possibility for this might to
        // implement a Callable and return the future produced instead, thought it'd be necessary to wrap it if we
        // still wish to apply timeouts on each separate invocation of Par.

        val start = System.nanoTime()
        val a = pa(es).get(l, timeUnit)
        val elapsed = System.nanoTime() - start
        val remaining = timeUnit.toNanos(l) - elapsed
        val b = pb(es).get(remaining, TimeUnit.NANOSECONDS)
        f(a, b)
      }
    }
  
  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /* Ex. 7.4 */
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  /* Ex. 7.5 */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldLeft(unit(List.empty[A]))((pz, pa) => map2(pz, pa)((z, a) => z :+ a))
  }

  /* Ex. 7.6 */
  // Option 1. Fork a new thread and map to par before calling sequence
  // Option 2. Split the list in the middle and evaluate
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    def go(la: List[A]): Par[List[A]] = la match {
      case l if (l.size > 1) =>
        val (left, right) = l.splitAt(l.size / 2)
        map2(go(left), go(right))(_ ++ _)
      case h :: Nil if f(h) =>
        lazyUnit(List(h))
      case _ =>
        lazyUnit(Nil)
    }
    fork { go(as) }
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Ex. 7.7 */
  // map(map(y)(g))(f) == map(y)(f compose g)
  // map(map(y)(g))(f) == map(y)(f(g(_))            // replace compose
  // map(map(y)(g))(f) == map(y)(f(map(_)(g)))      // g(y) -> map(y)(g)
  // map(map(y)(g))(f) == map(y)(map(map(_)(g))(f)) // f(y) -> map(y)(f)
  // map(map(y)(g))(f) == map(map(y)(g))(f)         // map(y)(f) -> f(y)

  /* Ex. 7.8 */
  // Using a single threaded executor service, the fact that we create a new callable to wrap around the Par that we
  // are submitting to the threadpool means we will encounter deadlock, and as such our Par will never complete and
  // thus does not satisfy the law!

  /* Ex 7.9 */
  // As we use map two to implement sequence, with map2 operations, that means for ever step in the fold we evaluate
  // two futures along with creating a new future that wraps the result that also gets executed. Therefore for a
  // threadpool of size n, any sequence of size n / 2 should result in a deadlock.

  /* Ex. 7.11.1 */
  def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] = {
    es =>
      val index = run(es)(p).get()
      run(es)(ps(index))
  }

  /* Ex. 7.11.2 */
  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    val index = map(a)(if(_) 0 else 1)
    choiceN(index)(List(ifTrue, ifFalse))
  }

  /* Ex. 7.12.1 */
  def choiceMap[K,V](p: Par[K])(ps: Map[K,Par[V]]): Par[V] = {
    es =>
      val key = run(es)(p).get
      run(es)(ps(key))
  }

  /* Ex. 7.13.1 */
  def chooser[A,B](p: Par[A])(f: A => Par[B]): Par[B] = {
    es =>
      f(run(es)(p).get)(es)
  }

  /* Ex. 7.13.2 */
  def choiceViaChooser[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
    chooser(p)(if (_) f else t)

  /* Ex. 7.13.3 */
  def choiceNViaChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(p)(choices(_))

  /* Ex. 7.14.1 */
  def join[A](p: Par[Par[A]]): Par[A] = {
    es =>
      val pa = run(es)(p).get()
      run(es)(pa)
  }

  /* Ex. 7.14.2 */
  def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] = chooser(p)(f)

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(pa => pa)

  /* Ex. 7.14.3 */
  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))



  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
