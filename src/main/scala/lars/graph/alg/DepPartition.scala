package lars.graph.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.strat.{DepGraph, grt}

import scala.collection.mutable

/**
 * Creates the Blocks for the StratumGraph. Similar like SCCFn (for determining the strongly connected components),
 * this function adds a block X its neighbour block Y, if there is no pair of nodes (x,y), where x in X and y in Y
 * s.t. a path from x to y exists with an edge labelled with `grt`.
 *
 * (This way we get a minimal stratum graph in terms of number of strata.)
 *
 * Created by et on 26.07.15.
 */
case class DepPartition() extends (DepGraph => Map[ExtendedAtom,Set[ExtendedAtom]]) {

  var keyCnt = -1 //maximal partition Index
  var block = new mutable.HashMap[Int, Set[ExtendedAtom]]()
  var partition = new mutable.HashMap[ExtendedAtom,Int]()

  override def apply(g: DepGraph): Map[ExtendedAtom, Set[ExtendedAtom]] = {
    var result = new collection.immutable.HashMap[ExtendedAtom,Set[ExtendedAtom]]


    for (node <- g.nodes) {
      if (!inBlock(node)) {
        if (canAddNodes(g,node)) {
/*          println("from graph: "+node)
          println("neighbour to: "+neighbourBlock(g,node))*/
          addNodes(node, g, neighbourBlock(g,node))
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
    for (n <- g.nodes) {
      if (g.adjList(n).contains(node)) return true
    }
    false
  }

  def canAddNodes(g: DepGraph, node: ExtendedAtom): Boolean = {
    g.outgoing(node).size match {
      case 0 => if (hasIncoming(g,node)) return false
      case 1 => {
        if (!hasIncoming(g,node)) return true
        if (isGrt(g,node,g.outgoing(node).head)) return false
      }
      case _ => return true
    }
    true
  }

  /* adds node to r
   *
   * pre: node is not in a block yet
   */
  def addNodes(node: ExtendedAtom, g: DepGraph, nb: Option[(Int, Set[ExtendedAtom])]) : Unit = {
      if (nb.isDefined && !hasSomePathWithGrt(g, nb.get._2, Set(node))) {
//        println("add "+node+" to existing blockSet")
        addNodeToBlock(g,nb.get._1,nb.get._2,node)
        return
      }
    keyCnt += 1
    block += (keyCnt -> Set())
    addNodeToBlock(g,keyCnt,block(keyCnt),node)
  }

  def addNodeToBlock(g: DepGraph, blockIndex: Int, blockSet: Set[ExtendedAtom], node: ExtendedAtom) : Unit = {
/*    println("block: "+block)
    println("partition: "+partition)
    println("node: "+node)
    println("hasGrtPath: "+hasSomePathWithGrt(g,blockSet,Set(node)))
    println("---")*/
   if (inBlock(node)) return // necessary?

     block(blockIndex) += node
     partition += (node -> blockIndex)
     addNeighbours(g, node)
  }

  def neighbourBlock(g: DepGraph, node: ExtendedAtom) : Option[(Int,Set[ExtendedAtom])] = {
//    println("\tblock: "+block)
    for ((key,value) <- block) {
      for (v <- value) {
//        println("outgoing: "+g.outgoing(v))
        if (g.outgoing(v).contains(node)) return Option((key,value))
      }
    }
   None
  }

  /* checks if the neighbours of node are already in r, and adds them if not */
  def addNeighbours(g: DepGraph, node: ExtendedAtom) = {
    for (n <- g.adjList(node)) {
      if (!inBlock(n) || isAloneInBlock(n)) {
        addNodes(n,g,Option(partition(node),block(partition(node))))
      } else if (partition(node) != partition(n)) {
        println("trying merge between "+partition(node)+" and "+partition(n))
        tryMerge(g,node,n)
      }
    }
  }

  /*checks if the set of block, which contains node, has any other elements in it
  * @return true if there are no other elements but node, false otherwise*/
  def isAloneInBlock(node: ExtendedAtom) : Boolean = {
    if (partition.contains(node) && block(partition(node)).size == 1) {
      block.remove(partition(node))
      partition.remove(node)
      return true
    }
    false
  }

  def tryMerge(g: DepGraph, node: ExtendedAtom, n: ExtendedAtom) : Unit = {

    val fromSet = block(partition(node))
    val fromKey = partition(node)

    val toSet = block(partition(n))
    val toKey = partition(n)

    if (!hasSomePathWithGrt(g,fromSet,toSet) && !hasSomePathWithGrt(g,toSet,fromSet))
      merge(g,fromKey,toKey)
  }

  def merge(g:DepGraph,fromKey: Int, toKey: Int) = {
    if (!hasGrtCycle(g,fromKey,toKey))
      block(math.min(fromKey,toKey)) ++= block(math.max(fromKey,toKey))
    for (v <- block(math.max(fromKey,toKey))) {
      partition(v) = math.min(fromKey,toKey)
    }
    block.remove(math.max(fromKey,toKey))

  }

  /* naive algorithm to check for cycles between the nodes of block */
  def hasGrtCycle(g:DepGraph, fromKey: Int, toKey: Int) : Boolean = {
    val tmp:Set[ExtendedAtom] = block(fromKey) ++ block(toKey)

    for ((ktmp,tmp) <- block) {
      for ((key, value) <- block) {
        if (ktmp != fromKey && key != toKey && ktmp != toKey && key != fromKey) {
          val edges = hasEdges(g, tmp, value)
          if (edges.nonEmpty && !hasGrtEdges(g, edges))
            if (hasSomePathWithGrt(g, tmp, value) || hasSomePathWithGrt(g, value, tmp)) return true
        }
      }
    }
    false
  }

  def hasEdges(g: DepGraph, tmp: Set[ExtendedAtom], value: Set[ExtendedAtom]): Set[(ExtendedAtom,ExtendedAtom)] = {
    var edgeSet:Set[(ExtendedAtom,ExtendedAtom)] = Set()
    for (t <- tmp) {
      for (v <- value) {
        if (g.hasEdge(t,v)) edgeSet = edgeSet ++ Set((t,v))
        else if (g.hasEdge(v,t)) edgeSet = edgeSet ++ Set((v,t))
      }
    }
    edgeSet
  }

  def hasGrtEdges(g: DepGraph, edges: Set[(ExtendedAtom, ExtendedAtom)]): Boolean = {
    for ((from,to) <- edges) {
      if (isGrt(g,from,to)) return true
    }
    false
  }

  /* checks if a node has already been added to r */
  def inBlock(node: ExtendedAtom) : Boolean = {
    if (partition.contains(node)) return true
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
        if (hasPathWithGrt(false, g, v1, v2, v1, marked) == (true,true)) {
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
  def hasPathWithGrt(foundGrtBefore: Boolean, g: DepGraph, v1: ExtendedAtom, v2: ExtendedAtom, last: ExtendedAtom, marked: collection.mutable.HashMap[ExtendedAtom, Boolean]): (Boolean,Boolean) = {
    marked(last) = true
    for (w <- g.outgoing(v1)) {
      if (!marked(w)) {
        val foundGreater = foundGrtBefore || isGrt(g, v1, w)
        if (w == v2) { //reached target
          return (foundGreater,true)
        }
        val foundGrtPath = hasPathWithGrt(foundGreater, g, w, v2, v1, markedCopy(marked))
        if (foundGrtPath._1 && foundGrtPath._2) {
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
