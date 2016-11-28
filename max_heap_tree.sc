import Node.addNode
import Node.creatTree
import Node.traversal
class Node(val value : Int,
           val superNode : Int= -1,
           val rightNodeindex :Int = -1,
           val leftNodeindex :Int = -1){

  def findend(tree : Array[Node], i :Int = 0) : Int =
    if (i == tree.length || tree(i) == null) i
    else findend(tree, i + 1)

  def findsurperNode(tree: Array[Node],end : Int, i:Int) :Int =
    if (tree(i).rightNodeindex == -1 && tree(i).rightNodeindex != -1) i
    else if (tree(i).rightNodeindex == -1 && tree(i).rightNodeindex == -1) i
    else findsurperNode(tree,end,i+1)

  def putintree(tree: Array[Node], point :Int) ={
    val end = findend(tree)

    val si = findsurperNode(tree,end,0)

    tree(end) = addNode(point,si)
    if (tree(si).leftNodeindex == -1)
      tree(si) = addNode(tree(si).value,tree(si).superNode,-1,end)
    else
      tree(si) = addNode(tree(si).value,tree(si).superNode,
        end,tree(si).leftNodeindex)
    sort(tree)
    //println(end,tree(end).value,si,
      //tree(si).leftNodeindex,tree(si).rightNodeindex)
  }
  def swap (tree : Array[Node], a :Int, b:Int):Boolean ={
    val temp = tree(a)
    tree(a) = addNode(tree(b).value,tree(a).superNode,
      tree(a).rightNodeindex,tree(a).leftNodeindex)
    tree(b) = addNode(temp.value,tree(b).superNode,
      tree(b).rightNodeindex,tree(b).leftNodeindex)
    true
  }
  def heapify(tree : Array[Node], i :Int):Int= {
    if (tree(i).leftNodeindex != -1 && tree(i).rightNodeindex != -1 &&
      tree(i).value < tree(tree(i).leftNodeindex).value &&
      tree(tree(i).rightNodeindex).value < tree(tree(i).leftNodeindex).value){
      swap(tree,i,tree(i).leftNodeindex)
      heapify(tree,tree(i).leftNodeindex)
    }

    else if (tree(i).leftNodeindex != -1 && tree(i).rightNodeindex != -1 &&
      tree(i).value < tree(tree(i).rightNodeindex).value &&
      tree(tree(i).leftNodeindex).value < tree(tree(i).rightNodeindex).value){
      swap(tree,i,tree(i).rightNodeindex)
      heapify(tree,tree(i).rightNodeindex)
    }

    else if (tree(i).leftNodeindex != -1 &&
      tree(i).value < tree(tree(i).leftNodeindex).value ){
      swap(tree,i,tree(i).leftNodeindex)
      heapify(tree,tree(i).leftNodeindex)
    }

    else -1

  }

  def sort(tree :Array[Node]) = {
    val iniporint = findend(tree)
    for (i <- (0 to tree(iniporint-1).superNode).reverse ){
      heapify(tree,i)

    }
  }
  def takeNode(tree :Array[Node],i : Int) :Int= {
    val v = tree(i).value
    val endpoint = tree(findend(tree)-1)
    if (findend(tree)-1==i) {
      if (i == 0) tree(i) = null
      else if (tree(tree(i).superNode).leftNodeindex == i)
        tree(tree(i).superNode) = addNode(tree(tree(i).superNode).value,
          tree(tree(i).superNode).superNode,
          tree(tree(i).superNode).rightNodeindex,-1)
      else if (tree(tree(i).superNode).rightNodeindex == i)
        tree(tree(i).superNode) = addNode(tree(tree(i).superNode).value,
          tree(tree(i).superNode).superNode,-1,
          tree(tree(i).superNode).leftNodeindex)

      tree(i) = null
      sort(tree)
      v
    }
    else {
      if (tree(tree(findend(tree)-1).superNode).leftNodeindex == findend(tree)-1)
        tree(tree(findend(tree)-1).superNode) = addNode(tree(tree(findend(tree)-1).superNode).value,
          tree(tree(findend(tree)-1).superNode).superNode,
          tree(tree(findend(tree)-1).superNode).rightNodeindex,-1)
      else if (tree(tree(findend(tree)-1).superNode).rightNodeindex == findend(tree)-1)
        tree(tree(findend(tree)-1).superNode) = addNode(tree(tree(findend(tree)-1).superNode).value,
          tree(tree(findend(tree)-1).superNode).superNode,-1,
          tree(tree(findend(tree)-1).superNode).leftNodeindex)

      tree(i) = addNode(endpoint.value,tree(i).superNode,
        tree(i).rightNodeindex,tree(i).leftNodeindex)

      tree(findend(tree)-1) = null
      sort(tree)
      v
    }
  }



}

object Node{

  def addNode(v:Int,rIndex:Int = -1,lIndex:Int = -1) =
    new Node(v,rIndex,lIndex)
  def addNode(v:Int,supIndex:Int,rIndex:Int,lIndex:Int) =
    new Node(v,supIndex,rIndex,lIndex)
  def creatTree(n:Int) = new Array[Node](n)
  def traversal(tree :Array[Node], i: Int = 0 ) :Int=
    if (i == -1) -1
    else {
      println(tree(i).value)
      traversal(tree,tree(i).leftNodeindex)
      traversal(tree,tree(i).rightNodeindex)
    }
}

val t = creatTree(12)
t(0) = addNode(26)

for (i <- 2 to 10)
  t(0).putintree(t,i)
for (i <- 0 to 9)
  println(t(i).value,t(i).leftNodeindex,t(i).rightNodeindex)

t(0).putintree(t,4)
t(0).putintree(t,18)
for (i <- 0 to 11)
  println(t(i).value,t(i).leftNodeindex,t(i).rightNodeindex)

t(0).takeNode(t,3)
t(0).takeNode(t,5)
t(0).takeNode(t,0)
for (i <- 0 to 8)
  println(t(i).value,t(i).leftNodeindex,t(i).rightNodeindex)

t(0).putintree(t,99)
t(0).putintree(t,4)
for (i <- 0 to 10)
  println(t(i).value,t(i).leftNodeindex,t(i).rightNodeindex)

traversal(t)