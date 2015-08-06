package lars.graph.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.graph.DiGraph
import lars.strat.{grt, DepGraph}

import scala.collection.mutable

/**
 * Created by et on 26.07.15.
 */
case class DepPartition() extends (DiGraph[ExtendedAtom] => Map[ExtendedAtom,Set[ExtendedAtom]]) {

  var r = new mutable.HashMap[Int, Set[ExtendedAtom]]()
  var greater= false
  var keyCnt = -1

  def apply(G: DiGraph[ExtendedAtom]): Map[ExtendedAtom, Set[ExtendedAtom]] = {
    var result = new collection.immutable.HashMap[ExtendedAtom,Set[ExtendedAtom]]

    var g:DepGraph = null

    G match {
      case a:DepGraph => g = a
      case _          => return null
    }

    for (node <- g.nodes) {
      if (!inSub(node)) {
        if (sat(g,node)) {
          addNodes(node, g)
        }
      }
    }
    for (node <- g.nodes) {
      for ((key, value) <- r) {
        if (value.contains(node)) {
          result += (node -> value)
        }
      }
    }
    result
  }

  def hasIncoming(g: DepGraph, node: ExtendedAtom) : Boolean = {
    for(n <- g.nodes){
      if(g.adjList(n).contains(node)) return true
    }
    false
  }

  def sat(g: DepGraph, node: ExtendedAtom): Boolean = {
    if (g.outgoing(node).isEmpty) return false
    if (g.outgoing(node).size == 1){
      if(!hasIncoming(g,node)) return true
      if(isGrt(g,node,g.outgoing(node).head)) return false
    }
    true
  }

  /*adds node to r*/
  def addNodes(node: ExtendedAtom, g: DepGraph) : Unit = {
    for ((key,value) <- r) {
      var n = isNeighbour(g,value,node)
      if(n != null) {
        if (!dfs(g, n, Set(node))) {
          //      if (!hasGrtPath(g, isNeighbour(g, value, node), node)) {
          println(r)
          println("node: "+node)
          r(key) += node
          println(r)
          addNeigh(g, node)
          return
        }
      }
    }
    keyCnt += 1
    r += (keyCnt->Set(node))
    addNeigh(g,node)
  }

  /*checks if node is a neighbour of any of the vertices in value
  * @return value, if node is a neighbour to any element of value, null otherwise*/
  def isNeighbour(g: DepGraph, value: Set[ExtendedAtom], node: ExtendedAtom): Set[ExtendedAtom] = {
    for (v <- value) {
      if (g.outgoing(v).contains(node)) return value
    }
   null
  }

  /*checks for every vertex of value if there is a path from node to the vertex containing a `grt` edge*/
/*  def hasGrtPath(g: DepGraph, value: Set[ExtendedAtom], node: ExtendedAtom) : Boolean = {
    if (value == null) return true
//    for (v <- value) {
        if (dfs(g, value, Set(node))) return true
//    }
    false
  }*/

  /*checks if the set of r, which contains node, has any other elements in it
  * @return true if there are no other elements but node, false otherwise*/
  def alone(node: ExtendedAtom) : Boolean = {
    for ((key,value)<-r) {
      if (value.contains(node) && value.size == 1) {
        r.remove(key)
        return true
      }
    }
    false
  }


  /*checks if the neighbours of node are already in r, and adds them if not*/
  def addNeigh(g: DepGraph, node: ExtendedAtom) = {
    for (n <- g.adjList(node)) {
      if (!inSub(n) || alone(n)) {
        addNodes(n, g)
      }/* else {
        tryMerge(g,node,n)
      }*/
    }
  }


  def tryMerge(g: DepGraph, node: ExtendedAtom, n: ExtendedAtom) : Unit = {
    println(r)
    println("node: "+node+" n: "+n)

    var fromSet, toSet:Set[ExtendedAtom] = null
    var fromKey, toKey = -1
    var grtP = false

    for ((key,value) <- r) {
      if (value.contains(node)) {
        fromKey = key
        fromSet = value
      }
      if (value.contains(n)) {
        toKey = key
        toSet = value
      }
    }
    if (fromKey == toKey) return
    println("fromK: "+fromKey+" toK: "+toKey)
/*    for (from <- fromSet) {
      for (to <- toSet) {
        println("from: "+from+" to: "+to)
        if (dfs(g,from,to) || dfs(g,to,from)) {
          grtP = true
        }
      }
    }*/
    if (dfs(g,fromSet,toSet) || dfs(g,toSet,fromSet)) {
      grtP = true
    }
    if (!grtP) merge(g,fromKey,toKey)
  }

  def merge(g:DepGraph,fromKey: Int, toKey: Int) = {
    if(!hasGrtCycle(g,fromKey,toKey))
      r(math.min(fromKey,toKey)) ++= r(math.max(fromKey,toKey))
    r.remove(math.max(fromKey,toKey))
  }


  def hasGrtCycle(g:DepGraph,fromKey: Int, toKey: Int) : Boolean = {
    var tmp:Set[ExtendedAtom] = r(fromKey) ++ r(toKey)
    var tToV, tToVG, vToT, vToTG = false
    var newR = collection.mutable.HashMap[Int,Set[ExtendedAtom]]()
    var cnt = 0

    for((key,value) <- r){
      cnt = key
      if(key != fromKey && key != toKey) {
        newR += (key->value)
      }
    }
    newR += (cnt+1->tmp)

    /*Loop-condition-party*/
    for((ktmp,tmp)<-newR) {
      for (t <- tmp) {
        for ((key, value) <- newR) {
          if (ktmp != key /*&& key != fromKey && key != toKey*/) {
            tToV = false
            tToVG = false
            vToT = false
            vToTG = false
            for (v <- value) {
              if (g.hasEdge(t, v)) {
                tToV = true
                if (isGrt(g, t, v)) {
                  tToVG = true
                }
              }
              if (g.hasEdge(v, t)) {
                vToT = true
                if (isGrt(g, v, t)) {
                  vToTG = true
                }
              }
            }
          }
          if ((tToVG && vToT) || (vToTG && tToV)) return true
        }
      }
    }


    false
  }


  /*checks if a node has already been added to r*/
  def inSub(node: ExtendedAtom) : Boolean = {
    for ((key,value) <- r) {
      if (value.contains(node)) {
        return true
      }
    }
    false
  }

  def isGrt(g: DepGraph, from: ExtendedAtom, to: ExtendedAtom): Boolean = {
    g.label(from, to) match {
      case `grt` => true
      case _ => false
    }
  }

  /*@return 1 if there is a path with a `grt` edge along the way, -1 otherwise*/
  def dfs(g: DepGraph, from: Set[ExtendedAtom], to: Set[ExtendedAtom]): Boolean = {
    greater = false
    val marked = new collection.mutable.HashMap[ExtendedAtom, Boolean]()
    for (n <- g.nodes) {
      marked(n) = false
    }

    for(v1 <- from){
      for(v2 <- to){
        if(!greater) check(greater, g, v1, v2, v1, marked)
      }
    }
    greater
  }

  /*creates a copy for every call of the check method, so all the paths get visited*/
  def markedCopy(marked: mutable.HashMap[ExtendedAtom, Boolean]): mutable.HashMap[ExtendedAtom, Boolean] = {
    val result = new mutable.HashMap[ExtendedAtom,Boolean]()
    for ((key,value)<-marked) {
      result += (key -> value)
    }
    result
  }

  /*sets the greater field to 1 (default -1), if it finds an with `grt`*/
  def check(grt: Boolean, g: DepGraph, v1: ExtendedAtom, v2: ExtendedAtom, last: ExtendedAtom, marked: collection.mutable.HashMap[ExtendedAtom, Boolean]): Unit = {
    marked(last) = true
      for (w <- g.outgoing(v1)) {
        var i = grt
        if (!marked(w)) {
          if (isGrt(g, v1, w)) i = true
          if (w == v2) greater = i
          val tmp = check(i,g, w, v2, v1, markedCopy(marked))
        }
      }
  }
}
