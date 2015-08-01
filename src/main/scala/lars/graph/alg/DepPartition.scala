package lars.graph.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.graph.{LabeledDiGraph, DiGraph}
import lars.strat.{Dependency, grt, DepGraph}

import scala.collection.mutable

/**
 * Created by et on 26.07.15.
 */
case class DepPartition() extends (DepGraph => Map[ExtendedAtom,Set[ExtendedAtom]]) {

  var r = new mutable.HashMap[Int, Set[ExtendedAtom]]()
  var greater= -1


  def sat(g: DepGraph, node: ExtendedAtom): Boolean = {
  if(g.outgoing(node).size == 1 && isGrt(g,node,g.outgoing(node).head)) return false
    true
  }

  def apply(g: DepGraph): Map[ExtendedAtom, Set[ExtendedAtom]] = {
    val nodes = g.nodes
    val adjList = g.adjList

    r += (0->Set())
      for (node <- nodes) {
        if (inSub(node) == -1) {
          if (g.outgoing(node).nonEmpty && sat(g,node)) {
            addNodes(node, g)
        }
      }
    }

    var result = new collection.immutable.HashMap[ExtendedAtom,Set[ExtendedAtom]]

    for (node <- g.nodes) {
      for ((key, value) <- r) {
        if (value.contains(node)) {
          result += (node -> value)
        }
      }
    }
    println(result)

    result
  }


  def isNeighbour(g: DepGraph, value: Set[ExtendedAtom], node: ExtendedAtom): Set[ExtendedAtom] = {
    for(v <- value){
      println("v: " + v + " ,node: " + node)
      if(g.outgoing(v).contains(node)) return value
    }
  null
  }

  def eval(g: DepGraph, value: Set[ExtendedAtom], node: ExtendedAtom) : Int = {
    if(value == null) return 1
    for(v <- value) {
      if(dfs(g, v, node) == 1) return 1
    }
    -1
  }

  def addNeigh(g: DepGraph, node: ExtendedAtom) = {
    for(n <- g.adjList(node)) {
      if (inSub(n) == -1) addNodes(n, g)
    }
  }

  def addNodes(node: ExtendedAtom, g: DepGraph) : Unit = {
    println(r)
    var b:ExtendedAtom = null
    var grtP = -1
    for((key,value) <- r){
      grtP = eval(g, isNeighbour(g, value, node), node)

      if(grtP == -1){
        r(key) += node
        addNeigh(g,node)
        return
      }
    }
    r += (r.size->Set(node))
    addNeigh(g,node)
  }

  def inSub(node: ExtendedAtom) : Int = {
    for ((key,value) <- r) {
      if (value.contains(node)) {
        return key
      }
    }
   -1
  }

  def isGrt(g: DepGraph, from: ExtendedAtom, to: ExtendedAtom): Boolean = {
    g.label(from, to) match {
      case `grt` => true
      case _ => false
    }
  }


  /*@return see check(g,v1,v2,marked)*/
  def dfs(g: DepGraph, v1: ExtendedAtom, v2: ExtendedAtom): Int = {
    greater = -1
    val marked = new collection.mutable.HashMap[ExtendedAtom, Boolean]()
    for (n <- g.nodes) {
      marked(n) = false
    }
    check(-1,g, v1, v2, v1, marked)
    greater
  }

  def markedCopy(marked: mutable.HashMap[ExtendedAtom, Boolean]): mutable.HashMap[ExtendedAtom, Boolean] = {
    val result = new mutable.HashMap[ExtendedAtom,Boolean]()
    for((key,value)<-marked){
      result += (key -> value)
    }
    result
  }

  /* @return true, if there is a ">" dependency on the path from v1 to v2, false otherwise */
  def check(grt: Int, g: DepGraph, v1: ExtendedAtom, v2: ExtendedAtom, last: ExtendedAtom, marked: collection.mutable.HashMap[ExtendedAtom, Boolean]): Unit = {
    var i = grt
    marked(last) = true
//    if(g.outgoing(v1).nonEmpty) {
      for (w <- g.outgoing(v1)) {
        if (!marked(w)) {
          if (isGrt(g, v1, w)) i = 1
          if (w == v2)  greater = i
          val tmp = check(i,g, w, v2, v1, markedCopy(marked))
        }
      }
//    }
  }
}
