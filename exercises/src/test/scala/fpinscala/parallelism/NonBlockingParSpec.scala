package fpinscala.parallelism

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ExecutorService, Executors, ThreadFactory, Future => JFuture}

import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{BeforeAndAfter, EitherValues, FlatSpec, Matchers}

import scala.concurrent.{Future => SFuture}

/**
  * Created by benen on 03/08/17.
  */
class NonBlockingParSpec extends FlatSpec with Matchers with BeforeAndAfter with EitherValues {

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

  behavior of "7.10 error handling"

  it should "register callbacks correctly" in {
    //given
    val succeeding = Nonblocking.Par.delay[Int](5)

    //When
    val result = Nonblocking.Par.run(executorService)(succeeding)

    // then
    result.right.value shouldBe 5
  }

  it should "recover from errors correctly" in {
    //given
    val failing = Nonblocking.Par.delay[Int]{throw new RuntimeException("Failing") }

    //When
    val result = Nonblocking.Par.run(executorService)(failing)

    // then
    result.left.value shouldBe a[RuntimeException]
  }
}
