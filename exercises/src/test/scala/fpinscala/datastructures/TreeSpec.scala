package fpinscala.datastructures

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}


/**
  * Created by benen on 29/07/17.
  */
class TreeSpec extends FlatSpec with PropertyChecks with Matchers {

  behavior of classOf[Tree[_]].getSimpleName

  val tree1: Tree[Int] = Leaf(1)
  val tree3: Tree[Int] = Branch(Leaf(1), Leaf(2))
  val tree7: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  val tree9: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))

  val sizedTrees = Table(
    ("tree", "Tree.size"),
    (tree1, 1),
    (tree3, 3),
    (tree7, 7),
    (tree9, 9)
  )

  it should "return the correct size of a tree" in {
    forAll(sizedTrees)((tree, expected) => Tree.size(tree) shouldEqual expected)
  }

  val maxedTrees = Table(
    ("tree", "Tree.max"),
    (tree1, 1),
    (tree3, 2),
    (tree7, 4),
    (tree9, 5)
  )

  it should "return the max value in a tree" in {
    forAll(maxedTrees)((tree, expected) => Tree.max(tree) shouldEqual expected)
  }

  val depthTrees = Table(
    ("tree", "Tree.depth"),
    (tree1, 1),
    (tree3, 2),
    (tree7, 3),
    (tree9, 4)
  )

  it should "return the correct depth of a tree" in {
    forAll(depthTrees)((tree, expected) => Tree.depth(tree) shouldEqual expected)
  }

  val mappedTrees = Table(
    ("tree", "Tree.map"),
    (tree1, Leaf(1)),
    (tree3, Branch(Leaf(1), Leaf(4))),
    (tree7, Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(9), Leaf(16)))),
    (tree9, Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(9), Branch(Leaf(16), Leaf(25)))))
  )

  it should "map a tree correctly" in {
    forAll(mappedTrees)((tree, expected) => Tree.map(tree)(x => x * x) shouldEqual expected)
  }

  it should "fold over a tree correctly" in {
    forAll(sizedTrees)((tree, expected) => Tree.sizeViaFold(tree) shouldEqual expected)
    forAll(maxedTrees)((tree, expected) => Tree.maxViaFold(tree) shouldEqual expected)
    forAll(depthTrees)((tree, expected) => Tree.depthViaFold(tree) shouldEqual expected)
    forAll(mappedTrees)((tree, expected) => Tree.mapViaFold(tree)(x => x * x) shouldEqual expected)
  }
}
