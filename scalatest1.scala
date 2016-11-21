/**
  * Created by louis on 2016/11/8.
  */

import scala.io.Source


object LongLines {
  def processFile(filename: String, width: Int) = {
    val source = Source.fromFile(filename)
    for (line <- source.getLines())
      processLine(filename, width, line)
  }
  private def processLine(filename: String,
                          width: Int, line: String) = {
    if (line.length > width)
      println(filename + ": " + line.trim)
  }
}

object FindLongLines {
  def main(args: Array[String]) = {
    println("hello")
    val width = args(0).toInt
    for (arg <- args.drop(1))
      LongLines.processFile(arg, width)

  }
}

/*
object test{
  def main (args: Array[String])={
    println(args(1))
    println("hello")
  }
}
*/