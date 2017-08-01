package fpinscala.datastructures

import org.scalacheck.Prop.{BooleanOperators, forAll, throws}
import org.scalacheck.{Gen, Properties}

import scala.util.Random

/**
  * Created by benen on 24/07/17.
  */
object ListSpec extends Properties("List") {

  property("tail of non empty list") = forAll(nonEmptyListGen) { list =>
    List.tail(list) == list.asInstanceOf[Cons[Int]].tail
  }

  property("tail of empty list") = forAll(emptyListGen) { list =>
    throws(classOf[IllegalArgumentException])(List.tail(list))
  }

  property("setHead of non empty list") = forAll(nonEmptyListGen) { list =>
    List.setHead(list, 17).asInstanceOf[Cons[Int]].head == 17
  }

  property("setHead of empty list") = forAll(emptyListGen) { list =>
    throws(classOf[IllegalArgumentException])(List.setHead(list, 17))
  }

  property("drop where size > n") = forAll(randomIntList, Gen.choose(0, 10)) { (list: List[Int], n: Int) =>
    (size(list) > n) ==> (size(List.drop(list, n)) == size(list) - n)
  }

  property("drop where n >= size") = forAll(randomIntList, Gen.choose(100, 1000)) { (list: List[Int], n: Int) =>
    (size(list) <= n) ==> (List.drop(list, n) == Nil)
  }

  property("dropWhile where size > n") = forAll(sizedListGen, Gen.choose(0, 10)) { (list: List[Int], n: Int) =>
    (size(list) > n) ==> (size(List.dropWhile(list, { (a: Int) => a <= n })) == size(list) - n)
  }

  property("dropWhile where n >= size") = forAll(sizedListGen, Gen.choose(100, 110)) { (list: List[Int], n: Int) =>
    (size(list) <= n) ==> (List.dropWhile(list, { (a: Int) => a <= n }) == Nil)
  }

  property("init of emptyList") = forAll(emptyListGen) { list =>
    throws(classOf[IllegalArgumentException])(List.init(list))
  }

  property("init of nonEmptyList") = forAll(nonEmptyListGen) { list =>
    size(List.init(list)) == size(list) - 1
  }

  property("length") = forAll(randomIntList) { list =>
    List.length(list) == size(list)
  }

  property("foldLeft should traverse all elements") = forAll(randomIntList) { list =>
    val newList = List.foldLeft(list, Nil: List[Int])((b, a) => Cons(a, b))
    size(newList) == size(list)
  }

  property("foldLeft should reduce to the same as foldRight") = forAll(randomIntList) { list =>
    List.foldLeft(list, 0)((z, _) => z + 1) == List.foldRight(list, 0)((_, z) => z + 1)
  }

  property("sumLeft should compute the sum of a list") = forAll(randomIntList) { list =>
    List.sumLeft(list) == List.sum(list)
  }

  property("product should compute the product of a list") = forAll(randomDoubleList) { list =>
    List.productLeft(list) ~= List.product(list)
  }

  property("reverse") = forAll(randomIntList) { list =>
    size(List.reverse(list)) == size(list)
  }

  property("foldLeft via foldRight") = forAll(randomIntList) { list =>
    List.foldLeft(list, Nil: List[Int])((t, h) => Cons(h, t)) ==
      List.foldLeftViaRight(list, Nil: List[Int])((t, h) => Cons(h, t))
  }

  property("foldRight via foldLeft") = forAll(randomIntList) { list =>
    List.foldRight(list, Nil: List[Int])((h, t) => Cons(h, t)) ==
      List.foldRightViaLeft(list, Nil: List[Int])((h, t) => Cons(h, t))
  }

  property("append via foldRight") = forAll(randomIntList, randomIntList) { (l1, l2) =>
    List.append(l1, l2) == List.appendViaFoldRight(l1, l2)
  }

  property("concat") = forAll((listOfLists)){ lists =>
    List.length(List.concat(lists)) == List.foldLeft(lists, 0){(c, a) => c + List.length(a)}
  }

  property("addOne") = forAll(randomIntList){ list =>
    List.sum(List.addOne(list)) == List.sum(list) + List.length(list)
  }

  property("asString") = forAll(randomDoubleList) { list =>
   List.length(List.asString(list)) == List.length(list)
  }

  property("map") = forAll(randomIntList){ list =>
    List.map(list)(identity) == list && List.map(list)(_ + 1) == List.addOne(list)
  }

  property("flatMap") = forAll(randomIntList){ list =>
    List.length(List.flatMap(list)(a => List(a, a))) == List.length(list) * 2
  }

  property("filterViaFlatMap") = forAll(randomIntList) { list =>
    def isEven: Int => Boolean = i => i % 2 == 0
    List.filter(list, isEven) == List.filterViaFlatmap(list, isEven)
  }

  property("addTwo") = forAll(sizedListGen) { list =>
    List.sum(List.addTwo(list, list)) == List.sum(list) * 2
  }

  property("zipWith") = forAll(sizedListGen){ list =>
    List.addTwo(list, list) == List.zipWith(list, list)(_ + _) &&
      List.length(List.zipWith(list, list)(_ + _)) == List.length(list)
  }

  property("hasSubsequence of nonEmptyList") = forAll(nonEmptyListGen, Gen.choose(1, 10)) { (list, n) =>
    val subSequence = List.drop(list, n)
    (n > 0) ==> (List.hasSubsequence(list, subSequence) == true)
  }


    def size[T](list: List[T]) = List.foldRight(list, 0)((_, a) => a + 1)

  def sizedListGen: Gen[List[Int]] = Gen.sized { size =>
    val values = for (i <- 1 to size) yield i
    List.apply(values: _*)
  }

  def randomIntList: Gen[List[Int]] = Gen.sized { size =>
    val values = for (_ <- 1 to size) yield Random.nextInt()
    List.apply(values: _*)
  }

  def randomDoubleList: Gen[List[Double]] = Gen.sized { size =>
    val values = for (_ <- 1 to size) yield Random.nextDouble()
    List.apply(values: _*)
  }

  def nonEmptyListGen: Gen[List[Int]] = Gen.sized { size =>
    val values = for (i <- 0 to size) yield Random.nextInt()
    List.apply(values: _*)
  }

  def listOfLists: Gen[List[List[Int]]] = Gen.sized { size =>
    val values = for (i <- 0 to size) yield Random.nextInt(100)
    val mapped = values.map { l =>
      val inner = for (j <- 0 to l) yield Random.nextInt()
      List.apply(inner: _*)
    }
    List.apply(mapped: _*)
  }

  def emptyListGen: Gen[List[Int]] = Gen.const(Nil)

  implicit class RichDouble(x: Double) {
    def ~=(y: Double) = if ((x - y).abs < 0.0001) true else false
  }

}
