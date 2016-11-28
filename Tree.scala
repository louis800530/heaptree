import scala.math.Ordered

/**
  * Created by louis on 2016/11/21.
  */
/*abstract class Tree[+T <% Ordered[T]] {
  val height: Int
}

sealed trait Node[+T] {
  def value: T
  def right: Tree[T]
  def left: Tree[T]
  def isEmpty: Boolean
  def fail(m: String) = throw new NoSuchElementException(m)
}

case class maxHeapTree[+T <% Ordered[T]](val value: T,
                                         val right: Tree[T] = Empty,
                                         val left: Tree[T] = Empty,
                                        val height: Int) extends Tree[T] with Node[T]{

  def isEmpty = false
  //val height = math.max(right.height,left.height) + 1
}
case object Empty extends Tree[Nothing] with Node[Nothing]{
  val value: Nothing = fail("empty!!!")
  val right: Nothing = fail("empty!!!")
  val left: Nothing = fail("empty!!!")
  def isEmpty = true
  val height = 0
}

object Tree{
  def empty[T]: Tree[T] = Empty
  def addNode[T <% Ordered[T]](v: T, r: Tree[T], l: Tree[T], h: Int): Tree[T] =
    maxHeapTree(v, r, l, math.max(r.height, l.height) + 1)
}



object GO{
  def main(args: Array[String]): Unit = {
    //val myTree = Tree.empty[Int]
    val myTree2 = Tree.addNode(7,Empty,Empty,1)
  }
}

*/