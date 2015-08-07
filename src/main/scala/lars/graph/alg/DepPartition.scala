package lars.graph.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.graph.DiGraph
import lars.strat.{grt, DepGraph}

import scala.collection.mutable

/**
 * Created by et on 26.07.15.
 */
case class DepPartition() extends (DiGraph[ExtendedAtom] => Map[ExtendedAtom,Set[ExtendedAtom]]) {

  var keyCnt = -1 //maximal partition Index
  var block = new mutable.HashMap[Int, Set[ExtendedAtom]]()
  //TODO inBlock map node 2 boolean
  var partition = new mutable.HashMap[ExtendedAtom,Int]() //TODO use instead of loops (key,value) <- block etc

  def apply(G: DiGraph[ExtendedAtom]): Map[ExtendedAtom, Set[ExtendedAtom]] = {
    var result = new collection.immutable.HashMap[ExtendedAtom,Set[ExtendedAtom]]

    var g:DepGraph = null

    G match {
      case a:DepGraph => g = a
      case _          => return null
    }

    for (node <- g.nodes) {
      if (!inBlock(node)) {
        if (canAddNodes(g,node)) {
          addNodes(node, g)
        }
      }
    }
    for (node <- g.nodes) {
      for ((key, value) <- block) {
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

  def canAddNodes(g: DepGraph, node: ExtendedAtom): Boolean = {
    if (g.outgoing(node).isEmpty) return false
    if (g.outgoing(node).size == 1){
      if (!hasIncoming(g,node)) return true
      if (isGrt(g,node,g.outgoing(node).head)) return false
    }
    //TODO = edges treated correctly?
    true
  }

  /* adds node to r
   *
   * pre: node is not in a block yet
   */
  def addNodes(node: ExtendedAtom, g: DepGraph) : Unit = {
    //TODO replace loop by findNeighbourBlock(g,node) (returns Option[Set[ExtendedAtom]])
    for ((key,value) <- block) {
      val b = neighbourBlock(g,value,node)
      if (b.isDefined) {
        if (!hasSomePathWithGrt(g, b.get, Set(node))) {
          block(key) += node
          addNeighbours(g, node)
          return
        }
      }
    }    
    keyCnt += 1
    block += (keyCnt -> Set(node))
    addNeighbours(g,node)
  }

  /*checks if node is a neighbour of any of the vertices in value
  * @return value, if node is a neighbour to any element of value, null otherwise*/
  def neighbourBlock(g: DepGraph, value: Set[ExtendedAtom], node: ExtendedAtom): Option[Set[ExtendedAtom]] = {
    for (v <- value) {
      if (g.outgoing(v).contains(node)) return Option(value)
    }
    None
  }

  /*checks if the set of r, which contains node, has any other elements in it
  * @return true if there are no other elements but node, false otherwise*/
  def isAloneInBlock(node: ExtendedAtom) : Boolean = {
    for ((key,value) <- block) {
      if (value.contains(node) && value.size == 1) {
        block.remove(key)
        return true
      }
    }
    false
  }

  /* checks if the neighbours of node are already in r, and adds them if not */
  def addNeighbours(g: DepGraph, node: ExtendedAtom) = {
    for (n <- g.adjList(node)) {
      if (!inBlock(n) || isAloneInBlock(n)) {
        addNodes(n, g)
      }/* else {
        tryMerge(g,node,n)
      }*/
    }
  }

  def tryMerge(g: DepGraph, node: ExtendedAtom, n: ExtendedAtom) : Unit = {
    println(block)
    println("node: "+node+" n: "+n)

    var fromSet, toSet:Set[ExtendedAtom] = null
    var fromKey, toKey = -1
    var grtP = false

    for ((key,value) <- block) {
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
    if (hasSomePathWithGrt(g,fromSet,toSet) || hasSomePathWithGrt(g,toSet,fromSet)) {
      grtP = true
    }
    if (!grtP) merge(g,fromKey,toKey)
  }

  def merge(g:DepGraph,fromKey: Int, toKey: Int) = {
    if (!hasGrtCycle(g,fromKey,toKey))
      block(math.min(fromKey,toKey)) ++= block(math.max(fromKey,toKey))
    block.remove(math.max(fromKey,toKey))
  }

  def hasGrtCycle(g:DepGraph, fromKey: Int, toKey: Int) : Boolean = {
    val tmp:Set[ExtendedAtom] = block(fromKey) ++ block(toKey)
    var tToV, tToVG, vToT, vToTG = false
    var newR = collection.mutable.HashMap[Int,Set[ExtendedAtom]]()
    var cnt = 0

    for ((key,value) <- block){
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


  /* checks if a node has already been added to r */
  def inBlock(node: ExtendedAtom) : Boolean = {
    for ((key,value) <- block) {
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

  /* @return true if there is a path with a `grt` edge along the way, false otherwise */
  def hasSomePathWithGrt(g: DepGraph, from: Set[ExtendedAtom], to: Set[ExtendedAtom]): Boolean = {
    val marked = new collection.mutable.HashMap[ExtendedAtom, Boolean]()
    for (n <- g.nodes) {
      marked(n) = false
    }
    for (v1 <- from) {
      for (v2 <- to) {
        if (hasPathWithGrt(false, false, g, v1, v2, v1, marked) == (true,true)) {
          return true
        }
      }
    }
    false
  }

  /**
   * sets the greater field to true (default false), if it finds a path with `grt`
   *
   * @return ._1: foundGrt, ._2:foundPath
   *
   */
  def hasPathWithGrt(foundGrtBefore: Boolean, foundPathBefore: Boolean, g: DepGraph, v1: ExtendedAtom, v2: ExtendedAtom, last: ExtendedAtom, marked: collection.mutable.HashMap[ExtendedAtom, Boolean]): (Boolean,Boolean) = {
    marked(last) = true
    for (w <- g.outgoing(v1)) {
      if (!marked(w)) {
        val foundGreater = foundGrtBefore || isGrt(g, v1, w)
        if (w == v2) { //reached target
          return (foundGreater,true)
        }
        val foundGrtPath = hasPathWithGrt(foundGreater, false, g, w, v2, v1, markedCopy(marked))
        if (foundGrtPath._2) {
          return foundGrtPath
        }
      }
    }
    (false,false)
  }

  /* creates a copy for every call of the check method, so all the paths get visited */
  def markedCopy(marked: mutable.HashMap[ExtendedAtom, Boolean]): mutable.HashMap[ExtendedAtom, Boolean] = {
    val result = new mutable.HashMap[ExtendedAtom,Boolean]()
    for ((key,value) <- marked) {
      result += (key -> value)
    }
    result
  }

}
