/**
  * Created by louis on 2016/11/25.
  */

sealed abstract class MaxHeapTree {
  def value: Int
  def right: MaxHeapTree
  def left: MaxHeapTree
  def isEmpty: Boolean
  def height: Int
}
object MaxHeapTree{
  def emptyTree: MaxHeapTree = Empty
  def make(v: Int, r: MaxHeapTree, l: MaxHeapTree): MaxHeapTree =
    new Node(v,r,l)
  def addPoint(a: Int, tree: MaxHeapTree): MaxHeapTree = tree match {
    case Empty => make(a,Empty,Empty)
    case Node(v, r, l) =>
      if (math.max(a, v) == a) {
        if (r.height <= l.height)
          make(a, addPoint(v, r), l)
        else
          make(a, r, addPoint(v, l))
      }
      else {
        if (r.height <= l.height)
          make(v, addPoint(a, r), l)
        else
          make(v, r, addPoint(a, l))
      }
  }
  def traversal(tree: MaxHeapTree): String=
    if (tree == Empty) "Done!"
    else{
      println(tree.value)
      traversal(tree.left)
      traversal(tree.right)
    }
  def takeRoot(tree: MaxHeapTree): MaxHeapTree = tree match {
    case Empty => Empty
    case Node(v, Empty, Empty) => Empty
    case Node(v, r, Empty) => make(r.value, takeRoot(r), Empty)
    case Node(v, Empty, l) => make(l.value, Empty, takeRoot(l))
    case Node(v, r, l) =>
      if (math.max(r.value,l.value) == r.value)
        make(r.value, takeRoot(r), l)
      else
        make(l.value, r, takeRoot(l))

  }
}
case class Node(val value: Int, val right: MaxHeapTree,
                val left: MaxHeapTree
               ) extends MaxHeapTree{
  def isEmpty = false
  def height: Int = math.max(right.height,left.height)
}
case object Empty extends MaxHeapTree{
  def value: Int = 0
  def right: MaxHeapTree = Empty
  def left: MaxHeapTree = Empty
  def isEmpty = true
  def height: Int = 0
}


    val t1 = MaxHeapTree.emptyTree
    val T1 = MaxHeapTree.addPoint(9,t1)
    val t2 = MaxHeapTree.make(3,Empty,Empty)
    val t3 = MaxHeapTree.addPoint(6,t2)

    val t4 = MaxHeapTree.addPoint(23,t3)
    MaxHeapTree.traversal(t4)
    println("---------------")
    val t5 = MaxHeapTree.takeRoot(t4)
    MaxHeapTree.traversal(t5)
    println("---------------")
