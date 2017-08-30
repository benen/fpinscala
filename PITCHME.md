# FP in Scala
### Ch. 7 Part two

###### Benen Cahill 
###### _31/08/2017_

Note:

---

### Recap

We've explored the process of designing a purely functional async library and made some explicit choices:
  1. Separating the description from the actual execution
  2. Emit absolutely **no side effects**
  3. Allow for combining asynchronous computations without blocking
  4. We've decided we want to allow for explicit forking

Note:

1. Our data type is `Par[A]` to describe an asynch op `A` and we access with `get(p: Par[A]): A`
2. We wont expose low level concurrency primitives that rely on side effects
3. Ensuring we can perform divide and conquer
4. Give clients a choice of how to combine asynchronous operations and when new thread spawned

---

### Recap

Through those decisions, we've encountered: 
* The difference between a primitive combinator and a derived combinator
* How our data type `Par[A]` has evolved from a container into something that's interpreted
* Seen how _effects_ can sometimes be unavoidable, but that they are different to _side effects_

Note:

1. `lazyUnit` vs. `fork` and `unit`
2. We've renamed our `get` to `run`
3. We only expose Future


---

### Recap

And settled on the representation: 

```scala
type Par[A] = ExecutorService => Future[A]

def unit[A]​(a: A): Par[A]
def fork[A]​(p: Par[A]): Par[A]
def map2[A, B, C]​(a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
def run[A]​(s: ExecutorService)(p: Par[A]): Future[A]
```

Note:

---


### Recap

By following the types, we can begin to think of our API as an algebra in the mathematical sense, with the following laws:
```scala
map(y)(id) = y

map(map(y)(g))(f) == map(y)(f compose g)

fork(x) = x
```

Note:

1. I.e. A set of functions that operate on a set according to some set of axioms (things we hold true)
2. Map fusion derived from _Free Theorems_
3. We discover how deadlock can break fork
4. This is something we need to do, due to composition

---

### The Problem with our API

```scala
def fork[A]​(a: => Par[A]): Par[A] = 
  es => es.submit(new Callable[A]) {
    def call = a(es).get
  }
```

Note:

1. We're submitting two `Callable`s, where one callable blocks on a thread! Wasting a thread!

---

### Working towards a Non-Blocking implementation

* We can't get a value out of a `Future` without blocking on get
* We have our laws which mean we only have to get it right once

Note:

1. We need to ensure our methods like `fork` and `map2` never call a method that blocks the current thread!

---

### Working towards a Non-Blocking implementation

Callbacks to the rescue
```scala
sealed trait Future[A] {
  private[parallism] def apply(k: A => Unit): Unit
}

type Par[+A] = ExecutorService => Future[A]

```

Note: 

1. We create a new future type that registers a callback
2. We ensure this is private to our library, such that we can be certain our API is pure
3. This is an example of local side effects, which are not exposed by our API. By ensuring that our laws still hold for
the exposed part of our API, we can be certain our side effects cannot be observed by consumers of our API

---

### Working towards a Non-Blocking implementation

```scala
def run[A]​​(es: ExecutorService)(p: Par[A]): A = {
  val ref = new AtomicReference[A]
  val latch = new CountDownLatch(1)
  p(es) { a => ref.set(a); latch.countDown }
  latch.await
  ref.get
}
```

Note: 

1. Run still blocks the calling thread (not possible to do anything else)
2. Execution of run is the only place this happens, so consumers simply delay its usage
3. We could go all the way and design `Par` with callbacks

--- 

### Working towards a Non-Blocking implementation

```scala
def unit[A]​​(a: A): Par[A] = 
  es => new Future[A] {
    def apply(cb: A => Unit): Unit = cb(a)
  }

def fork[A]​​(a: => Par[A]): Par[A] =
  es => new Future[A] {
    def apply(cb: A => Unit): Unit =
      eval(es)(a(es)(cb))
  }
  
def eval(es: ExecutorService)(r: => Unit): Unit = 
  es.submit(new Callable[Unit] { def call = r })
```
  
Note: 

1. Unit simply applies the callback, disregarding the ExecutorService
2. When the `Future` created by fork receives its callback, it starts evaluating a in a separate thread. The resulting
   `Future` has the callback registered.
3. Map2 is significantly more tricky to write, the potential race conditions require many low level primitives.  
   
---
   
### Introducing Actors

A concurrent process that only occupies a thread when needed
* An actor has a mailbox and receives messages from potentially many different sources
* An actor may have many messages in its mailbox, but only processes one message at time
* An actor processes messages in the order it receives them

Note: 

* No race conditions to worry about
* By processing one message at time we make it easy to avoid deadlock

### Introducing Actors

---

Creating an actor: 
```scala
val echoer = Actor[String](Executors.newFixedThreadPool){
   msg => println(s"Got message '$msg'")
}
```
Using an actor: 
```scala
scala> echoer ! "Hello"
Got message: 'Hello'
```

---

### Implementing Map2

```scala
def map2[A,B,C]​​(p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
  es => new Future[C] {
    def apply(cb: C => Unit): Unit = {
      var ar: Option[A] = None
      var br: Option[B] = None
      val combiner = Actor[Either[A,B]](es) {
        case Left(a) =>
          if (br.isDefined) eval(es)(cb(f(a,br.get)))
          else ar = Some(a)
        case Right(b) =>
          if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
          else br = Some(b)
      }
      p(es)(a => combiner ! Left(a))
      p2(es)(b => combiner ! Right(b))
    }
  }
```

Note:

1. It's too liberal forking threads, it forks not just a separate thread for the combiner but also the 

---

### Exercise 7.10

_Hard:_ Our non-blocking representation doesn't currently handle errors at all. If at any point our computation throws
an exception, the run implementation's latch never counts down and the exception is simply swallowed. Can you fix that? 

---

### The Power of Laws

Not only do they enshrine a citizen's rights, but they can also keep a programmer in a job!
* Without our fork law, we may not have found that resource leak
* Laws don't just validate our design, they help us reason about it

---

## Generalising Combinators

* As we develop an API, it's common to come up with new combinators as you tackle specific use cases.
* But quite often taking this approach, we will end up repeating ourselves while missing out on commonalities
* A good exercise to to try and identify combinators _in their most general form_ for your API. 

---

### Exercise 7.11

Implement choiceN and then `choice` in terms of `choiceN`

---

### Exercise 7.12

```scala
def choiceMap[K, V]​​(key: Par[K])(choices: Map[K, Par[V]]): Par[V]
```

Come up with a more general combinator with which you can implement `choice`, `choiceN` and `choiceMap` 

---

### Exercise 7.13

```scala
def chooser[A, B]​​(pa: Par[A])(choices: A => Par[B]): Par[B]
```

Implement the new primitive `chooser`, and then use it to implement `choice` and `choiceN`

---

### Generalising Further

Chooser is now so general, chooser is not really an appropriate name for it. 

```scala
def chooser[A, B]​​(pa: Par[A])(choices: A => Par[B]): Par[B]
```

In essence, given the result of a parallel computation it generates a new one from it. It may not be the case that we
are choosing anything, the resulting `Par[B]` could be created entirely based on the value of `A`. Indeed, this type
of operation is called `bind` in Haskell and `flatMap` in most Scala libraries. 

---

### Generalising Further

Is `flatMap` really the most general primitive we can come up with though? By definition it involves two operations:

```scala
def map[A, B]​​(p: Par[A])(f: A => B): Par[B]

def join[A]​​(p: Par[Par[A]]): Par[A] // or flatten
```

---

### Exercise 7.14

Implement join. Can you see how to implement `flatMap` using `join`? And can you implement `join` using `flatMap`? 

---

### Summary

Note:

---

### Scala vs. Twitter Futures

What is a `Future`? A data structure for time travel

* `Future` is a container for some value that will be computed in the future
* Idea is that the evaluation of the value represented will be computed asynchronously and gathered at a later point in 
time

```scala
trait Future[A] { 
  def get: A
}
```
---

### Scala vs. Twitter Futures

* In the dawn of time (Scala 2.9), there was no `Future` implementation within the Scala standard library
* With the explosion of asynchronous frameworks, many communities implemented their own versions:
  * Akka
  * Finagle
  * Scalaz
* The flame war was long and arduous, with many casualties. Ultimately though, the Akka version prevailed.
* `Futures` were integrated with the Scala standard library (SIP-14)
  * Though, like `Hiroo Onoda`, Twitter kept fighting long after the truce was declared...

---
  
### Scala vs. Twitter Futures

Twitter Futures are cancellable
```
val f: Future[_] = ... // some long running operation
f.interrupt(new TimeoutException("Taking too long")
```

Note: 
* Basically dispatches the interrupt to the Promise completing the future. Doesn't change the observable
state of the Future from the outside, just tells the promise that we're no longer interested in this future's
results for whatever reason
* Not referentially transparent!

---

### Scala vs. Twitter Futures

Twitter Futures can be delayed
```
import com.twitter.conversions.time._
f.delay(1 second)
```

---

### Scala vs. Twitter Futures

Twitter's onSuccess composes sequentially

```scala
f.onSuccess { case _ => println("First")}.onSuccess { case _ => println("Second") }
```

Note: 
* Scala's onSuccess returns unit, so you can't guarantee a sequential result this way. But you
  should be using flatMap for this use case anyway!

---

### Scala vs. Twitter Futures

Twitter's future supports continuation local variables (analogous to java's Thread Local)

```scala
val l: Local[Int] = new Local[User]
f.onSuccess { case name => l() = new User(name) }
```

Note: 
* Very low level and should be avoided!

---

### Scala vs. Twitter Futures

Twitter futures have non standard naming conventions: 
* `join` vs `zip`
* `rescue` vs `recover`

---
 
### Scala vs. Twitter Futures

Twitter futures use a different dispatch mechanism. 
* Scala: someone else do it (i.e. another Thread, dispatched using an Executor)
* Twitter: I'll do it later

Note: 
* Scala's mechanism has a fixed cost, but can be quite expensive as we assign pass resources to another thread
* Originally some cost attached to registering callbacks that could result in stack-overflow, this has been fixed
* Twitter's mechanism is stack safe, but note they may not asynchronous! It's essentially just a notification that
this value may or may not be asynchronous. In order to actually execute something asynchronously, we use `FutureTask`
* Scala forces you to use ExecutionContext, Twitter is just a representation

---

### Scala vs. Twitter Futures

Conclusion:
* Twitter Futures are sort of a hangover
* Neither offer the granular control around execution that our Par library does!


