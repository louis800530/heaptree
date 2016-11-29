/**
  * Created by louis on 2016/11/25.
  */

sealed abstract class MaxHeapTree {
  def value: Option[Double]

  def right: MaxHeapTree

  def left: MaxHeapTree

  def height: Int

  import MaxHeapTree.apply

  def addPoint(a: Double): MaxHeapTree = this match {
    case Empty => apply(a, Empty, Empty)
    case Node(v, r, l) =>
      if (a > v.get) {
        if (r.height <= l.height) apply(a, r.addPoint(v.get), l)
        else apply(a, r, l.addPoint(v.get))
      }
      else {
        if (r.height <= l.height) apply(v.get, r.addPoint(a), l)
        else apply(v.get, r, l.addPoint(a))
      }
  }
  def traversal: String =
    if (this == Empty) "Done!"
    else {
      println(this.value.get)
      this.left.traversal
      this.right.traversal
    }

  def removeRoot: MaxHeapTree = this match {
    case Empty => Empty
    case Node(v, Empty, Empty) => Empty
    case Node(v, r, l) =>
      if (l.value.isEmpty || (r.value exists(x => x > l.value.get)))
        apply(r.value.get, r.removeRoot, l)
      else apply(l.value.get, r, l.removeRoot)
  }

  override def toString: String = "MaxHeapTree"
}

object MaxHeapTree {
  def emptyTree: MaxHeapTree = Empty

  def apply(v: Double, r: MaxHeapTree, l: MaxHeapTree): MaxHeapTree = new Node(Some(v), r, l)
}

case class Node(value: Option[Double],
                right: MaxHeapTree,
                left: MaxHeapTree) extends MaxHeapTree {

  def height: Int = math.max(right.height, left.height) + 1

  override def toString: String = "MaxHeapTree of Root: " + value.get
}

case object Empty extends MaxHeapTree {
  def value: Option[Nothing] = None

  def right = throw new NoSuchElementException("EmptyTree.right")

  def left = throw new NoSuchElementException("EmptyTree.left")

  def height: Int = 0

  override def toString: String = "Empty"
}

object Go {
  def main(args: Array[String]): Unit = {
    val t1 = MaxHeapTree.emptyTree

    val t2 = MaxHeapTree(3, Empty, Empty)

    val t3 = t2 addPoint 6

    val t4 = t3 addPoint 23
    t4.traversal
    println("---------------")

    val t5 = t4.removeRoot
    t5.traversal
    println("---------------")

    val t6 = t4 addPoint 13
    println(t6.traversal)
    println("---------------")

    println(t6)
    println(t6.height)
    println(t2.removeRoot)
  }
}
