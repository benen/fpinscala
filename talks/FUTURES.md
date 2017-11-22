## Scala vs. Twitter Futures
#### A quick summary of the differences

###### Benen Cahill 
###### _31/08/2017_

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
  
---  

### Scala vs. Twitter Futures
  
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

Twitter futures have some different naming conventions: 
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
* Twitter's mechanism is stack safe, but note they may not be asynchronous! It's essentially just a notification that
this value may or may not be asynchronous. In order to actually execute something asynchronously, we use `FutureTask`
* Scala forces you to use ExecutionContext, Twitter is just a representation

---

### Scala vs. Twitter Futures

Conclusion:
* Twitter Futures are sort of a hangover
* Neither offer the granular control around execution that our Par library does!


