trait Queue[T] {
  def head: T
  def tail: Queue[T]
  def enqueue(x: T): Queue[T]
}
object Queue {
  def apply[T](xs: T*): Queue[T] =
    new QueueImpl[T](xs.toList, Nil)
  private class QueueImpl[T](
                              private val leading: List[T],
                              private val trailing: List[T]
                            ) extends Queue[T] {
    def mirror =
      if (leading.isEmpty)
        new QueueImpl(trailing.reverse, Nil)
      else
        this
    def head: T = mirror.leading.head
    def tail: QueueImpl[T] = {
      val q = mirror
      new QueueImpl(q.leading.tail, q.trailing)
    }
    def enqueue(x: T) =
      new QueueImpl(leading, x :: trailing)
  }
}
object Sort {
  def quick(list: List[Int]): List[Int] = {
    list match {
      case Nil => Nil
      case x::xs =>
        val (before,after) = xs partition (_ < x)
        quick(before) ++ (x :: quick(after))
    }
  }
}
def maxListOrdering[T](elements: List[T])
                      (implicit ordering: Ordering[T]): T =
  elements match {
    case List() =>
      throw new IllegalArgumentException("empty list!")
    case List(x) => x
    case x :: rest =>
      val maxRest = maxListOrdering(rest)(ordering)
      if (ordering.gt(x, maxRest)) x
      else maxRest
  }
maxListOrdering(List(2,4,6,8,10))
import Sort._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
quick(List(4,6,9,1,0,3,4,7))
val ss = "123 992".toArray
ss.toList

val sq: ArrayBuffer[Int] = scala.collection.mutable.ArrayBuffer.empty[Int]
val rk = Array("123 456 983 532")



val fi = "334 55,44".split(Array(' ',','))
fi.foreach( f =>   sq += f.toInt)
val n = sq.toArray

n.foreach(println)


type Y =Int
def Ytest(y: Y) = (y: Y) => println(y+4)
def Xtest = (x: Y) => println(x+4)
//val increase = (x: Y) => x + 9999
//println(increase(3))
val yy = Ytest(8)
println(yy(9))
println("------")
Xtest(8)

val a = List(1,2,3,4,5)
val b = List(5,6,7,8,9)
a zip b

(a zip b).map(f => f._1*f._2)
case class ball1(b1: Int) extends ball
case class ball2(b2: ball) extends ball
class ball extends Traversable[Int]{
  def foreach[U](f: Int=> U) = this match {
    case ball1(b1) => f(b1)
    case ball2(b2) => b2 foreach f
  }
}
