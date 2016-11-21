
class Rational(n: Int, d: Int) {
  require(d != 0)
  private val g = gcd(n.abs, d.abs)
  val nu = n/g
  val de = d/g
  def this(n: Int) = this(n, 1)
  override def toString = nu + "/" + de

  def +(a :Rational) :Rational=
    new Rational(n* a.de +d*a.nu,d*a.de)
  def + (i: Int): Rational =
    new Rational(nu + i * de, de)
  def * (that: Rational): Rational =
    new Rational(nu * that.nu, de * that.de)
  def - (that: Rational): Rational =
    new Rational(
      nu * that.de - that.nu * de,
      de * that.de
    )
  def - (i: Int): Rational =
    new Rational(nu - i * de, de)
  def * (i: Int): Rational =
    new Rational(nu * i, de)
  def / (that: Rational): Rational =
    new Rational(nu * that.de, de * that.nu)
  def / (i: Int): Rational =
    new Rational(nu, de* i)
  def one:Rational=
    new Rational(nu,de)
  def twice:Rational=
    new Rational(nu*de+de*nu,de*de)
  def lessThan(that: Rational) =
    this.nu * that.de < that.nu * this.de
  def lessThan2(that: Rational) =
    nu * that.de < that.nu * de
  def max(that: Rational) =
    if (this.lessThan(that)) that else this
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
}
implicit def rrr(x: Int) = new Rational(x)

val t = new Rational(-7,8)
val x = new Rational(5,9)
val y = new Rational(4,3)

//val dou = y.one

val z = x.+(y)
val m = x*t
val w = x + z +3

3+z-(x*7)
y lessThan x
y lessThan2(x)
x lessThan y
x lessThan2 t
x max t
val filesHere = (new java.io.File(".")).listFiles
for (file <- filesHere if file.getName.endsWith(".scala"))
  println(file)

def fileLines(file: java.io.File) =
  scala.io.Source.fromFile(file).getLines().toList
def grep(pattern: String) =
  for (
    file <- filesHere
    if file.getName.endsWith(".scala");
    line <- fileLines(file)
    if line.trim.matches(pattern)
  ) println(file + ": " + line.trim)
grep(".*gcd.*")

val rg = List(1 ,3, 5)
rg foreach println
def ok = for (i <- 1 to 5  if i%2 == 0;
              j <- -1 to 5 if j%2 == 0 )
 yield i*j

ok
val n =4
val half =
  if (n % 2 == 0)
    n/2 else
    throw new RuntimeException("n must be even")





// Returns a row as a sequence
def makeRowSeq(row: Int) =
for (col <- 1 to 10) yield {
  val prod = (row * col).toString
  val padding = " " * (4 - prod.length)
  padding + prod
}
// Returns a row as a string
def makeRow(row: Int) = makeRowSeq(row).mkString
// Returns table as a string with one row per line
def multiTable() = {
  val tableSeq = // a sequence of row strings
    for (row <- 1 to 10)
      yield makeRow(row)
  tableSeq.mkString("\n")
}

print(multiTable)
val basenum =List(1,2,3,4,5,6,7,8,9)
val li =List(1,9,5,35,-8,81)
val EW = (vrx:Int ,vry:Double) =>vrx*vry
val WT = (_:Int)+(_:Int)
val addd = ad _
def ad(x:List[Int] ,y:Int*) ={
  for (bas<-x;inp<-y)
    println(bas*inp)
  0
}
//ad(basenum,li: _*)
//ad(basenum,-8,6,221)

//addd(li)
//addd(basenum,li)
if (0 == addd(basenum,List(-4,5,2)))
  println("right")
//li.foreach(println)
EW        (3,5.7)


val oo =7
oo==fggg
def fggg =7



object Person {
  var counter: Int = 0
  def printStatus() {
    println("counter:" + counter)
  }
}
class Person(val userID: Int, var name: String) {
  Person.counter += 1
}

val person = new Person(1,"akk")
val person2 = new Person(2,"ugg")
Person.printStatus()

def find(x:Any) = x match{
  case List(2,9,4,_*) => println("find 2 out!")
  case (2,b,"RRRRRR") => println( "match" + b )
  case s: String => println(s)
  case List (_,_,8,goal@_*) => println(goal)
  case _ => println("there is no 2")
}
find(List(2,9,4,72,2))
find(Nil)
find(())
find(null)
//find(Option[Int])
find((2,"wow","RRRRRR"))
find("yyyyaaaa")
val a1 =2
val a2 ="tuple"
val a3 ="RRRRRR"
find((a1,a2,a3))
find(List(-3,6,8,2,3,4,5,10))
case class point4d(val x:Double,
              val y:Double,
              val z:Double,
              val w:Double){

}
object point4d{
  def cri( x:Double,
   y:Double,
   z:Double,
   w:Double) :point4d= new point4d(x,y,z,w)
}
def find4dpoint(point:Any) = point  match{

  case List(point4d(1,2,3,4),point4d(9,8,7,6)) =>
    point4d(5,6,7,8)
  case p: List[point4d] => p
  case _ => println("not found")
}
import point4d.cri
val k= cri(1,2,3,4)
val k2=cri(9,2,7,6)
//find4dpoint(k)
val l = List(k,k2)
find4dpoint(l)
l.head
l.tail.head
l.isEmpty
val ll=l:::l

def sp (t:List[Int],r:List[Int])= t match {
  case at::bt => bt
  case _ =>
}
sp(List(3,7,11),List(2,6,8))







def isort(xs: List[Int]): List[Int] = xs match {
  case List()   => List()
  case x :: xs1 => insert(x, isort(xs1))
}
def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List()  => List(x)
  case y :: ys => if (x <= y) x :: xs
  else y :: insert(x, ys)
}
isort(List(9,3,2,1,4,33,34,5,3))

def append[T](xs: List[T], ys: List[T]): List[T] =
  xs match {
    case List() => ys
    case x :: xs1 => x :: append(xs1, ys)
  }
append(List(1,(),5,()),List((),4,3,()))

val ty::uy =List((),2,3)
ty
uy

List(())==()
List()==()
List()==List(())
()
List().length
List(()).length
List(())(0)
