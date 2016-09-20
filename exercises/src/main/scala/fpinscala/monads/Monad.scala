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

  /* Ex 11.7 */
  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = a =>
      flatMap(f(a))(g)

  /* Ex 11.8 */
  def flatMapViaCompose[A,B](ma: M[A])(f: A => M[B]): M[B] =
      compose({ _: Unit => ma }, f)()

  /* Ex 11.9 */
  // compose(compose(f, g), h): A => M[D] == compose(f, compose(g, h))
  // a => flatMap(compose(f, g)(a))(h)    == a => flatMap(f(a))(b => compose(g, h)(b))
  // a => flatMap(flatMap(f(a))(g))(h)    == a => flatMap(f(a))(b => flatMap(g(b))(h))
  // substitute f(a) for x and we have the same as the listing in the book
  // flatMap(flatMap(x)(g))(h)            == flatMap(x)(b => flatMap(g(b))(h))

  /* Ex 11.10 */
  // compose(f, unit)            == f == compose(unit, f)
  // a => flatMap(f(a))(unit(_)) == f == a => flatMap(unit(a)(f)
  // let f(a) == x
  // flatMap(x)(unit)            == f == a => flatMap(unit(a))(f)

  /* Ex 11.11 */
  // let's use the option monad, with f: A => M[List[A]]
  // compose(f, unit): Option[A] => M[List[A]] == compose(unit, f): Option[A] => M[List[A]]

  /* Ex 11.12 */
  def join[A](mma: M[M[A]]): M[A] =
   flatMap(mma){ ma: M[A] => ma }

  /* Ex 11.13 i */
  def composeViaJoinAndMap[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = a =>
    join(map(f(a))(g))

  /* Ex 11.13 ii */
  def flatMapViaJoinAndMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))

  /* Ex 11.14 */
  // Associativity
  // flatMap(flatMap(x)(f))(g)    == flatMap(x)(a => flatMap(f(a))(g))
  // join(map(join(map(x)(f))(g)) == join(map(x)(a => join(map(f(a))(g))))
  // Identity
  // join(map(join(map(x))(unit))(f)) == join(map(join(map(x))(f))(unit))
}

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

  /* Ex 11.17 iii */
  lazy val idMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma.flatMap(f)
    override def unit[A](a: => A): Id[A] = Id(a)
  }

  def getState[S]: State[S,S] = State(s => (s,s))
  def setState[S](s: S): State[S,Unit] = State(_ => ((),s))

  /* Ex 11.20 iii */
  def readerMonad[R]: Monad[({type f[x] = Reader[R,x]})#f] = Reader.readerMonad[R]
}

case class Id[A](value: A) {
  /* Ex 11.17 i */
  def map[B](f: A => B): Id[B] = Id(f(value))

  /* Ex 11.17 ii */
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    /* Ex 11.20 i */
    override def unit[A](a: => A): Reader[R,A] = Reader(_ => a)

    /* Ex 11.20 ii */
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = Reader({ r =>
      val a = st.run(r)
      f(a).run(r)
    })
  }
}

