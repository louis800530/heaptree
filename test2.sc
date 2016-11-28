val listn = List(3,7,11,4,8,123,54,93,-4)
listn mkString ("[","o","]")
val buf = new StringBuilder
listn addString (buf,"(", ";", ")")
def msort[T](less: (T, T) => Boolean)
            (xs: List[T]): List[T] = {
  def merge(xs: List[T], ys: List[T]): List[T] =
    (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (less(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (ys, zs) = xs splitAt n
    merge(msort(less)(ys), msort(less)(zs))
  }
}
List.range(1 , 5) flatMap(
  i => List.range(1, i) map (j => (i, j)))
listn.find(_ >3).get
listn.filter(_ >4)
def add17(ori:List[Int]): Int = (ori.head /: ori.tail)(_ + _ )
add17(listn)
listn.foldRight(listn.head)(_ + _)
def reverseLeft[T](xs: List[T]) =
  (List[T]() /: xs) {(ys, y) => y :: ys}
reverseLeft(listn)
for (i <- Range(3,-1,-1)) println(i)

