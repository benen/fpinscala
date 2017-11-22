# Functional Programming in Scala
### Ch. 9: Algebraic Design

###### Benen Cahill 
###### _23/11/2017_

Note:

The chapter is called Parser Combinators, but really it's all about 
formalizing the functional design process we've explored earlier into
a laws first _algebraic design_ methedology. Try not to get bogged 
down on the detail of the actual parser implementation.

+++

### What's in this chapter?

We're going to use _Algebraic Design_ to implement a combinator library for parsers
1. Design our interfaces first and formalize laws about them
2. Use our design to inform our data structures

In otherwords, we're going to start with the algebra first this time

+++

### What's an _Algebra_?

- A collection of functions operating over some data types
- A set of laws specifying the relationships between these functions

+++

### Design Goals

- Expressiveness
- Performance
- Good Error Reporting

+++

### Designing Our Algebra

We're going to work our way from the bottom, starting simple with single characters

```scala
def char(c: Char): Parser[Char]

def run[A](p: Parser[A])(input: String): Either[ParseError,A]
```
@[1]
@[3]


Note: 

We don't care what Parser and ParseError are. We're just defining an interface

+++

### Designing Our Algebra

In fact, we can formalize our interface using a trait

```scala
trait Parsers[ParseError, Parser[+_]] {
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def char(c: Char): Parser[Char]
}
```

Note:

This is possible due to Scala's type system.

+++

### Designing Our Algebra

Our first interface has some obvious laws:

```scala
run(char(c))(c.toString) == Right(c)
```

+++

### Designing Our Algebra

We can add some more functions to generate parsers

```scala
def string(s: String): Parser[String]
def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
```

With their laws

```scala
run(string(s))(s) == Right(s)
run(or(string("abra"),string("cadabra")))("abra") == Right("abra")
run(or(string("abra"),string("cadabra")))("cadabra") == Right("cadabra")
```

+++

### Designing Our Algebra

```scala
trait Parsers[ParseError, Parser[+_]] { self =>
  ...
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  case class ParserOps[A](p: Parser[A]) { 
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
  }
}
```
@[3]
@[7-10]
@[5]


Note: Infix Notation. We use implicits to allow us to write parserA or parserB

+++

### Designing Our Algebra

What about repetition? We can easily add a combinator for it

```scala
def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
```
@[1]
@[3-5]

+++

### Designing Our Algebra

**Team Exercise**: Explore how we can expand our algebra to include:

1. Parsing zero or more characters
2. Parsing one or more strings
3. Zero or more digits _and_ one or more letters

+++

### Designing Our Algebra

**Team Exercise**: Also take into consideration:

1. How can we obtain length efficiently?
2. Is repetition a primitive?
3. How can we let the programmer control errors?
4. What additional laws can we think of?

+++

### A Possible Algebra

```scala
def many[A](p: Parser[A]): Parser[List[A]] 

def map[A,B](a: Parser[A])(f: A => B): Parser[B]

val numA: Parser[Int] = char('a').many.map(_.size) // obtaining the number of chars

map(p)(a => a) == p // map should conform to the following
```
@[1]
@[3]
@[5]
@[7]

Note: mapping the list is inefficient to get length

+++

### A Possible Algebra

What laws should the above conform to?

```scala
import fpinscala.testing._

object Laws {
  def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
    forAll(in)(s => run(p1)(s) == run(p2)(s))
  def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
    equal(p, p.map(a => a))(in)
}
```

+++

### A Possible Algebra

```scala
def char(c: Char): Parser[Char] =
  string(c.toString) map (_.charAt(0))
  
def succeed[A](a: A): Parser[A] =
  string("") map (_ => a)  
  
```
@[1-2]
@[4-5]

+++

### A Possible Algebra

```scala
def slice[A](p: Parser[A]): Parser[String]

def many1[A](p: Parser[A]): Parser[List[A]]

def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)]
```

@[1]
@[3]
@[5]

+++

### A Possible Algebra

**Exercises**:

- Ex. 9.1: Implement `map2` in terms of `product`, and then `many1` in terms of `many`
- Ex. 9.2: Identify laws that specific the behavior of product
- Ex. 9.3: Define `many` in terms of `or`, `map2` or `succeed`
- Ex. 9.4: Using `map2` and `succeed`, implement `listOfN`

+++

### A Possible Algebra

Now we can implement `many`

```scala
def many[A](p: Parser[A]): Parser[List[A]] =
  map2(p, many(p))(_ :: _) or succeed(List())
```

But there's a problem with this...

+++

### A Possible Algebra

We need to make the second arguments of `product` and `map2` non-strict 

```scala
def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]

def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
  product(p, p2) map (f.tupled)
```

@[1]
@[3-4]

+++

### Handling Context Sensitivity

So far we've introduced _repetition_, combinators such as `map2` and `product`, and different branches by providing `or`.
But as of yet, we've not been able to provide context sensitivity, i.e. given a result of `A` then parse `B`. We need 
a new primitive. We could call it `andThen` but more typically in scala this context sensitivity is provided by `flatMap`.

```scala
def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
```  

+++

### Handling Context Sensitivity

**Exercises**:

- Ex. 9.5: Using `flatMap` implement a context sensitive parser to parse zero or more digits followed by many letters
- Ex. 9.6: Implement `product` and `map2` in terms of `flatMap`
- Ex. 9.7: Implement `map` using `flatMap` and other combinators

+++

### Handling Context Sensitivity

Now that we've implemented `flatMap`, we find we can express some of our previous primitives in terms of it. This 
reduces our list of primitives that we must implement down to:
- `string`
- `regex`
- `slice`
- `succeed`
- `or`
- `flatMap`

+++

### Implementing A JSON Parser

Even though we don't have a concrete implementation of our parser yet, we can still implement our JSON parser. We can
write it as a function like the following:
```scala
def jsonParser[Err,Parser[+_]](P: Parsers[Err,Parser]): Parser[JSON] = {
  import P._
  val spaces = char(' ').many.slice
  ...
}
```

+++

### Implementing A JSON Parser

We'll introduce a simple syntax tree for our `JSON` type
```scala
trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}
```

+++

### Implementing A JSON Parser

**Exercises**:

- Ex. 9.9: Using the primitives we defined earlier, implement `Parser[JSON]`


+++

### Error Handling

If we were to go straight to the implementation right now, we'd be making some pretty arbitray decisions around
error handling. We'll start by looking at error reporting in the same context as the API

+++

### Error Handling

**Exercises**:

- Ex. 9.10: Spending some time discovering a nice set of combinators for errors

+++ 

### Error Handling

```scala
def label[A](msg: String)(p: Parser[A]): Parser[A]
```

+++

### Error Handling

```scala
case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }
}

def errorLocation(e: ParseError): Location
def errorMessage(e: ParseError): String
```

+++

### Error Handling

```scala
def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
  forAll(inputs ** Gen.string) { case
    (input, msg) =>
      run(label(msg)(p))(input) match {
        case Left(e) => errorMessage(e) == msg
        case _=> true
      }
  }
```

+++

### Error Handling: Nesting

```scala
val p = label("first magic word")("abra") **
  " ".many **
  label("second magic word")("cadabra")
  
run(p)("abra cAdabra")
```
@[1-3]
@[5]

+++

### Error Handling: Nesting

```scala
def scope[A](msg: String)(p: Parser[A]): Parser[A]

case class ParseError(stack: List[(Location,String)])
```
@[1]
@[3]

+++

### Error Handling: Branching

```scala
val spaces = " ".many
val p1 = scope("magic spell") {
  "abra" ** spaces ** "cadabra"
}
val p2 = scope("gibberish") {
  "abba" ** spaces ** "babba"
}
val p = p1 or p2
```

Note:

If we successfully parse magic spell, we become committed to the branch. 
We may want to introduce a combinator to such effect.

+++

### Error Handling: Branching

```scala
def attempt[A](p: Parser[A]): Parser[A]

attempt(p flatMap (_ => fail)) or p2 == p2
```
@[1]
@[3]

+++ 

### Error Handling: Branching

**Exercises**:

- Ex. 9.11: Can you think of any other combinators we might want for branching?

+++

### Implementing the Algebra

- `string(s)`
- `regex(s)`
- `slice(p)`
- `label(e)(p)`
- `scope(e)(p)`
- `flatMap(p)(f)`
- `attempt(p)`
- `or(p1, p2)`

+++

### Implementing the Algebra

**Exercises**:

- Ex. 9.12: Come up with ideas for a parser representation

+++

### Implementing the Algebra

```scala
type Parser[+A] = String => Either[ParseError,A]

def string(s: String): Parser[A] =
  (input: String) =>
    if (input.startsWith(s))
      Right(s)
    else
      Left(Location(input).toError("Expected:"+s)
```
@[1]
@[3-8]

+++

### Implementing the Algebra

The previous representation has a problem: We've no way to specify how to sequence parsers!

- We can solve this by introduceing a Result type that tracks characters consumed
- Caller then is just required to update the location state

+++

### Implementing the Algebra

```scala
type Parser[+A] = Location => Result[A]

trait Result[+A]
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError) extends Result[Nothing]
```
@[1]
@[3-5]

+++ 

### Implementing the Algebra

**Exercises**:

- Ex. 9.13: Implement `string`, `regex`, `succeed` and `slice`

+++

### Implementing the Algebra: Labelling

```scala
def push(loc: Location, msg: String): ParseError =
  copy(stack = (loc,msg) :: stack)

def scope[A](msg: String)(p: Parser[A]): Parser[A] = 
  s => p(s).mapError(_.push(s.loc,msg))

def mapError(f: ParseError => ParseError): Result[A] = this match {
  case Failure(e) => Failure(f(e))
  case _ => this
}
```
@[1-2]
@[3-4]
@[7-10]

+++

### Implementing the Algebra: Labelling

```scala
def label[A](msg: String)(p: Parser[A]): Parser[A] =
  s => p(s).mapError(_.label(msg))
```

+++

### Implementing the Algebra: Labelling

```scala
def label[A](s: String): ParseError =
  ParseError(latestLoc.map((_,s)).toList)

def latestLoc: Option[Location] =
  latest map (_._1)

def latest: Option[(Location,String)] =
  stack.lastOption
```

+++

### Implementing the Algebra: Labelling

**Exercises**:

- 9.14: Revise your implementation of string to use scope and/or label to provide a meaningful error message in the event of an error. 

+++

### Implementing the Algebra: Failover and Backtracking

```scala
case class Failure(get: ParseError, isCommitted: Boolean)
  extends Result[Nothing]
```

+++

### Implementing the Algebra: Failover and Backtracking

```scala
def attempt[A](p: Parser[A]): Parser[A] =
  s => p(s).uncommit

def uncommit: Result[A] = this match {
  case Failure(e, true) => Failure(e, false)
  case _ => this
}
```

+++

### Implementing the Algebra: Failover and Backtracking

```scala
def or[A](x: Parser[A], y: => Parser[A]): Parser[A] = s => x(s) match { 
  case Failure(e,false) => y(s)
  case r => r
}
```

+++

### Implementing the Algebra: Context Sensitive Parsing

```scala
def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
  s => f(s) match {
    case Success(a,n) => g(a)(s.advanceBy(n))
                           .addCommit(n != 0)
                           .advanceSuccess(n)
    case f@Failure(_,_) => f
  }
```

+++

### Implementing the Algebra: Context Sensitive Parsing

```scala
// defined on Location
def advanceBy(numChars: Int): ParseState =
  copy(loc = loc.copy(offset = loc.offset + numChars))
  
// defined on Result
def addCommit(isCommitted: Boolean): Result[A] = this match {
  case Failure(e,c) => Failure(e, c || isCommitted)
  case _ => this
}
def advanceSuccess(n: Int): Result[A] = this match {
  case Success(a,m) => Success(a,n+m)
  case _ => this
}
```
@[1-3]
@[5-9]
@[10-14]

+++

### Implementing the Algebra

**Exercises**:

- Ex. 9.15: Implement the rest of the primitives, includoing `run`
- Ex. 9.16: Format `ParseError` in a nice way for human consumption
- Ex. 9.17: Can we make `slice` more efficient (i.e. so we don't build up lists and lists?)?
- Ex. 9.18: `or` only keeps failures from the second branch. Can we improve this to report the branch that got furthest?

+++

### Summary

- Algebraic Design
  - Think about your interfaces and specify your laws before worrying about an implementation
  - Start with the simplest use cases you can think of!
  - Let the ideal API guide the implementation 
- This is something that can be brought to anybodies daily work!









