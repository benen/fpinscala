package fpinscala.parallelism

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Callable, ExecutorService, Executors, ThreadFactory, TimeUnit, TimeoutException, Future => JFuture}

import scala.concurrent.{Future => SFuture}
import scala.concurrent.ExecutionContext.Implicits.global
import fpinscala.parallelism.Par.Par
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

/**
  * Created by benen on 03/08/17.
  */
class ParSpec extends FlatSpec with PropertyChecks with Matchers with BeforeAndAfter with ScalaFutures with Eventually {

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

  behavior of "7.3 map2WithTimeouts"

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

  behavior of "7.4 asyncF"

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

  behavior of "7.5 sequence"

  it should "wrap the results in a single par" in {
    // given
    val list = List(asPar(1), asPar(2), asPar(3))

    // when
    val result = Par.sequence(list).run

    // then
    whenReady(result.asScala){ inner => inner shouldEqual List(1, 2, 3) }
  }

  behavior of "7.6 filter"

  it should "filter the list in parallel" in {
    forAll(Gen.listOf(Arbitrary.arbitrary[Int])){ list =>
      def isEven: Int => Boolean = _ % 2 == 0
      list.filter(isEven) shouldEqual Par.parFilter(list)(isEven).get
      assertAsync
    }
  }

  behavior of "7.11.1 choiceN"

  it should "work asynchronously for non-empty Lists" in {
    val intPars = List(Par.lazyUnit(1), Par.lazyUnit(2), Par.lazyUnit(3))
    val n = Par.lazyUnit(1)
    val parInt = Par.choiceN(n)(intPars).run
    eventually {
      assert(parInt.get == 2)
      assertAsync
    }
  }

  behavior of "7.11.2 choiceViaChoiceN"

  it should "work asynchronously for true case" in {
    val trueChoice = Par.choiceViaChoiceN(Par.lazyUnit(true))(Par.lazyUnit("yes"), Par.lazyUnit("no")).run
    eventually {
      assert(trueChoice.get == "yes")
      assertAsync
    }
  }

  it should "work asynchronously for false case" in {
    val falseChoice = Par.choiceViaChoiceN(Par.lazyUnit(false))(Par.lazyUnit("yes"), Par.lazyUnit("no")).run
    eventually {
      assert(falseChoice.get == "no")
      assertAsync
    }
  }

  behavior of "7.12 choiceMap"

  it should "work asynchronously" in {
    val choices = Map(1 -> Par.lazyUnit(1), 2 -> Par.lazyUnit(2), 3 -> Par.lazyUnit(3))
    val key = Par.lazyUnit(2)
    val parInt = Par.choiceMap(key)(choices).run
    eventually {
      assert(parInt.get == 2)
      assertAsync
    }
  }

  behavior of "7.13.1 chooser"

  it should "work asynchronously for List" in {
    val intPars = List(Par.lazyUnit(1), Par.lazyUnit(2), Par.lazyUnit(3))
    val n = Par.lazyUnit(1)
    val parInt = Par.chooser(n)(intPars).run
    eventually {
      assert(parInt.get == 2)
      assertAsync
    }
  }

  it should "work asynchronously for Map" in {
    val choices = Map(1 -> Par.lazyUnit(1), 2 -> Par.lazyUnit(2), 3 -> Par.lazyUnit(3))
    val key = Par.lazyUnit(2)
    val parInt = Par.chooser(key)(choices).run
    eventually {
      assert(parInt.get == 2)
      assertAsync
    }
  }

  behavior of "7.13.2 choiceViaChooser"

  it should "work asynchronously for true case" in {
    val trueChoice = Par.choiceViaChooser(Par.lazyUnit(true))(Par.lazyUnit("yes"), Par.lazyUnit("no")).run
    eventually {
      assert(trueChoice.get == "yes")
      assertAsync
    }
  }

  it should "work asynchronously for false case" in {
    val falseChoice = Par.choiceViaChooser(Par.lazyUnit(false))(Par.lazyUnit("yes"), Par.lazyUnit("no")).run
    eventually {
      assert(falseChoice.get == "no")
      assertAsync
    }
  }

  behavior of "7.13.3 choiceNViaChooser"

  it should "work asynchronously for List" in {
    val intPars = List(Par.lazyUnit(1), Par.lazyUnit(2), Par.lazyUnit(3))
    val n = Par.lazyUnit(1)
    val parInt = Par.choiceNViaChooser(n)(intPars).run
    eventually {
      assert(parInt.get == 2)
      assertAsync
    }
  }

  behavior of "7.14.1 join"

  it should "work asynchronously" in {
    val ppi = Par.lazyUnit(Par.lazyUnit(42))
    val i = Par.join(ppi)
    eventually {
      assert(i.get == 42)
      assertAsync
    }
  }

  behavior of "7.14.2 flatMapViaJoin"

  it should "work asynchronously" in {
    val pi = Par.lazyUnit(42)
    val i = Par.flatMapViaJoin(pi)(Par.lazyUnit(_))
    eventually {
      assert(i.get == 42)
      assertAsync
    }
  }

  behavior of "7.14.3 joinViaFlatMap"

  it should "work asynchronously" in {
    val ppi = Par.lazyUnit(Par.lazyUnit(42))
    val i = Par.joinViaFlatMap(ppi)
    eventually {
      assert(i.get == 42)
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
