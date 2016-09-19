package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  /* Ex 11.3 i */
  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldLeft(unit(List.empty[A])){ (mla, ma) => map2(mla, ma)((la, a) => la :+ a) }

  /* Ex 11.3 ii */
  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldLeft(unit(List.empty[B])){ (mlb, a) => map2(mlb, f(a))((lb, b) => lb :+ b) }

  /* Ex 11.4 */
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    def go(mla: M[List[A]], nn: Int): M[List[A]] =
      if (nn > 0)
        go(map2(mla, ma)(_ :+ _), nn - 1)
      else
        mla
    go(unit(List.empty[A]), n)
  }

  /* Ex 11.6 */
  def filterM[A](la: List[A])(f: A => M[Boolean]): M[List[A]] = la match {
    case Nil => unit(Nil)
    case x :: xs => map2(map(f(x))(if (_) List(x) else List()),  filterM(xs)(f))(_ ++ _)
  }

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = ???

  def composeViaJoinAndMap[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = ???

  // Implement in terms of `compose`:
  def flatMapViaCompose[A,B](ma: M[A])(f: A => M[B]): M[B] = ???

  def join[A](mma: M[M[A]]): M[A] = ???

  // Implement in terms of `join`:
  def flatMapViaJoinAndMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  /* Ex 11.1 i */
  lazy val parMonad: Monad[Par] = new Monad[Par] {
    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(ma)(f)
    override def unit[A](a: => A): Par[A] = Par.unit(a)
  }

  /* Ex 11.1 ii */
  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] = p.flatMap(ma)(f)
    override def unit[A](a: => A): P[A] = p.succeed(a)
  }

  /* Ex 11.1 iii */
  lazy val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap f
    override def unit[A](a: => A): Option[A] = Some(a)
  }

  /* Ex 11.1 iv */
  lazy val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma flatMap f
    override def unit[A](a: => A): Stream[A] = Stream(a)
  }

  /* Ex 11.1 v */
  lazy val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f
    override def unit[A](a: => A): List[A] = List(a)
  }

  /* Ex 11.2 */
  // Type signature already given. But basically you construct an existential type that conforms to State[S, A]
  def stateMonad[S]: Monad[({type lambda[x] = State[S, x]})#lambda] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
    override def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma.flatMap(f)
    override def unit[A](a: => A): State[S, A] = State.unit(a)
  }

  lazy val idMonad: Monad[Id] = ???

  def getState[S]: State[S,S] = State(s => (s,s))
  def setState[S](s: S): State[S,Unit] = State(_ => ((),s))

  def readerMonad[R]: Monad[({type f[x] = Reader[R,x]})#f] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = ???
  def flatMap[B](f: A => Id[B]): Id[B] = ???
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = ???
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = ???
  }
}

