package fpinscala.parallelism

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Callable, ExecutorService, Executors, ThreadFactory, TimeUnit, TimeoutException, Future => JFuture}

import scala.concurrent.{Future => SFuture}
import scala.concurrent.ExecutionContext.Implicits.global
import fpinscala.parallelism.Par.Par
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

/**
  * Created by benen on 03/08/17.
  */
class ParSpec extends FlatSpec with PropertyChecks with Matchers with BeforeAndAfter with ScalaFutures {

  val asyncThreadCount = new AtomicInteger
  val threadFactory: ThreadFactory =
    (r: Runnable) => {
      asyncThreadCount.incrementAndGet
      Executors.defaultThreadFactory.newThread(r)
    }

  var executorService: ExecutorService = _

  before {
    asyncThreadCount.set(0)
    // note that sequence() will not work if we do not provide enough parallel threads! (see exercise 7.9)
    executorService = Executors.newCachedThreadPool(threadFactory)
  }

  behavior of "Par.map2WithTimeouts"

  it should "apply f correctly" in {
    // given
    val pa = Par.delay { TimeUnit.MILLISECONDS.sleep(10); Par.unit(1) }
    val pb = Par.delay { TimeUnit.MILLISECONDS.sleep(10); Par.unit(2) }

    // when
    val result = Par.map2WithTimeouts(pa, pb)(_ + _)(executorService).get(25L, TimeUnit.MILLISECONDS)

    // then
    result shouldEqual 3
  }

  it should "respect timeouts" in intercept[TimeoutException] {
    // given
    val pa: Par[Int] = asPar { TimeUnit.MILLISECONDS.sleep(3); 1 }
    val pb: Par[Int] = asPar { TimeUnit.MILLISECONDS.sleep(3); 1 }

    // when
    Par.map2WithTimeouts(pa, pb)(_ + _)(executorService).get(5L, TimeUnit.MILLISECONDS)

    // then BOOM!
  }

  behavior of "Par.asyncF"

  it should "apply f in a separate thread" in {
    // given
    val op = Par.asyncF((i: Int) => i.toString)(42)

    // when
    val running = op.run

    // then
    whenReady(running.asScala){ result =>
      result shouldEqual "42"
      assertAsync
    }
  }

  behavior of "Par.sequence"

  it should "wrap the results in a single par" in {
    // given
    val list = List(asPar(1), asPar(2), asPar(3))

    // when
    val result = Par.sequence(list).run

    // then
    whenReady(result.asScala){ inner => inner shouldEqual List(1, 2, 3) }
  }

  it should "always deadlock" in {
    // given
    val gen: Gen[(Int, List[Par[Int]])] = for {
      n <- Gen.choose(2, Int.MaxValue)
      l <- Gen.listOfN(n / 2, Arbitrary.arbitrary[Int])
    } yield (n, l.map(asPar(_)))


    // when
    forAll(gen){case (n, l) =>
      val es = Executors.newFixedThreadPool(n)
      a[TimeoutException] should be thrownBy Par.sequence(l)(es)
    }

  }

  behavior of "Par.filter"

  it should "filter the list in parallel" in {
    forAll(Gen.listOf(Arbitrary.arbitrary[Int])){ list =>
      def isEven: Int => Boolean = _ % 2 == 0
      list.filter(isEven) shouldEqual Par.parFilter(list)(isEven).get
      assertAsync
    }
  }


  private def assertAsync = assert(asyncThreadCount.get > 0, "execution must be async")

  private def assertSync = assert(asyncThreadCount.get == 0, "execution must be sync")

  private def asPar[A](a: => A): Par[A] = { (es) =>
    val exe = new Callable[A] { def call(): A = a }
    es.submit(exe)
  }

  implicit class TestParOps[A](p: Par[A]) {
    def run: JFuture[A] = Par.run(executorService)(p)
    def get: A = Par.run(executorService)(p).get
  }

  implicit class ToScalaFuture[A](f: JFuture[A]) {
    def asScala: SFuture[A] = SFuture { f.get }
  }

}
