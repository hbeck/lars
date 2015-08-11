package lars.graph.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.graph.DiGraph
import lars.strat.{DepGraph, grt}

import scala.collection.mutable
import scala.collection.immutable

/**
 * Creates the Blocks for the StratumGraph. Similar like SCCFn (for determining the strongly connected components),
 * this function adds a block X to its neighbour block Y, if there is no pair of nodes (x,y), where x in X and y in Y
 * s.t. a path from x to y exists with an edge labelled with `grt`.
 *
 * (This way we get a minimal stratum graph in terms of number of strata.)
 *
 * Created by et on 26.07.15.
 */
case class DepPartition(g: DepGraph) extends ((DiGraph[Set[ExtendedAtom]]) => Map[Set[ExtendedAtom],Set[Set[ExtendedAtom]]]) {


  var keyCnt = -1 //maximal partition Index
  var block = new mutable.HashMap[Int, Set[Set[ExtendedAtom]]]()
  var partition = new mutable.HashMap[Set[ExtendedAtom],Int]()

  var grtEdges: Map[Set[ExtendedAtom],Set[ExtendedAtom]] = null

  override def apply(dg: DiGraph[Set[ExtendedAtom]]): Map[Set[ExtendedAtom],Set[Set[ExtendedAtom]]] = {
    var result = new collection.immutable.HashMap[Set[ExtendedAtom],Set[Set[ExtendedAtom]]]

    grtEdges = getGrtEdges(g,dg)

    for (node <- dg.nodes) {
      if (!inBlock(node)) {
        if (canAddNodes(g,dg,node)) {
          addNodes(dg, node, neighbourBlock(dg,node))
        }
      }
    }
    var tmp = new mutable.HashMap[Int, Set[ExtendedAtom]]()
    for ((key, value) <- block) {
      for (node <- block(key)) {
        result += (node -> block(key))
      }
    }
    result
  }
  
  def getGrtEdges(g: DepGraph, dg: DiGraph[Set[ExtendedAtom]]): Map[Set[ExtendedAtom],Set[ExtendedAtom]] = {
    var result = new mutable.HashMap[Set[ExtendedAtom],Set[ExtendedAtom]]
    for (node <- g.nodes) {
      for (n <- g.adjList(node)) {
        g.label(node, n) match {
          case `grt` => {
            var from, to = Set[ExtendedAtom]()
            for (vertex <- dg.nodes) {
              if (vertex.contains(node)) from = vertex
              else if (vertex.contains(n)) to = vertex
            }
            result += (from -> to)
          }
          case _ => result
        }
      }
    }
    result.toMap
  }

  def canAddNodes(g: DepGraph, dg: DiGraph[Set[ExtendedAtom]], node: Set[ExtendedAtom]): Boolean = {
    dg.outgoing(node).size match {
      case 0 => if (hasIncoming(dg,node)) return false
      case 1 => {
        if (!hasIncoming(dg,node)) return true
        if (isGrt(/*g,dg,*/node,dg.outgoing(node).head)) return false
      }
      case _ => return true
    }
    true
  }

  def hasIncoming(dg: DiGraph[Set[ExtendedAtom]], node: Set[ExtendedAtom]) : Boolean = {
    for (n <- dg.nodes) {
      if (dg.adjList(n).contains(node)) return true
    }
    false
  }

  /* adds node to block
   *
   * pre: node is not in a block yet
   */
  def addNodes(dg: DiGraph[Set[ExtendedAtom]], node: Set[ExtendedAtom], nb: Option[(Int, Set[Set[ExtendedAtom]])]) : Unit = {
      if (nb.isDefined && !hasSomePathWithGrt(dg, nb.get._2, Set(node))) {
//        println("add "+node+" to existing blockSet")
        addNodeToBlock(dg,nb.get._1,nb.get._2,node)
        return
      }
    keyCnt += 1
    block += (keyCnt -> Set())
    addNodeToBlock(dg,keyCnt,block(keyCnt),node)
  }

  def addNodeToBlock(dg: DiGraph[Set[ExtendedAtom]], blockIndex: Int, blockSet: Set[Set[ExtendedAtom]], node: Set[ExtendedAtom]) : Unit = {
   if (inBlock(node)) return // necessary?

     block(blockIndex) += node
     partition += (node -> blockIndex)
     addNeighbours(dg, node)
  }

  def neighbourBlock(dg: DiGraph[Set[ExtendedAtom]], node: Set[ExtendedAtom]) : Option[(Int,Set[Set[ExtendedAtom]])] = {
//    println("\tblock: "+block)
    for ((key,value) <- block) {
      for (v <- value) {
//        println("outgoing: "+g.outgoing(v))
        if (dg.outgoing(v).contains(node)) return Option((key,value))
      }
    }
   None
  }

  /* checks if the neighbours of node are already in r, and adds them if not */
  def addNeighbours(dg: DiGraph[Set[ExtendedAtom]], node: Set[ExtendedAtom]) = {
    for (n <- dg.adjList(node)) {
      if (!inBlock(n) || isAloneInBlock(n)) {
        addNodes(dg,n,Option(partition(node),block(partition(node))))
      } else if (partition(node) != partition(n)) {
        println("trying merge between "+partition(node)+" and "+partition(n))
        tryMerge(dg,node,n)
      }
    }
  }

  /*checks if the set of block, which contains node, has any other elements in it
  * @return true if there are no other elements but node, false otherwise*/
  def isAloneInBlock(node: Set[ExtendedAtom]) : Boolean = {
    if (partition.contains(node) && block(partition(node)).size == 1) {
      block.remove(partition(node))
      partition.remove(node)
      return true
    }
    false
  }

  def tryMerge(dg: DiGraph[Set[ExtendedAtom]], node: Set[ExtendedAtom], n: Set[ExtendedAtom]) : Unit = {

    val fromSet = block(partition(node))
    val fromKey = partition(node)

    val toSet = block(partition(n))
    val toKey = partition(n)

    if (!hasSomePathWithGrt(dg,fromSet,toSet) && !hasSomePathWithGrt(dg,toSet,fromSet))
      merge(dg,fromKey,toKey)
  }

  /* naive algorithm to check for cycles between the nodes of block */
  def merge(dg: DiGraph[Set[ExtendedAtom]], fromKey: Int, toKey: Int) : Boolean = {
    val undo = new mutable.HashMap[Int, Set[Set[ExtendedAtom]]]()
    undo += (fromKey -> block(fromKey),toKey -> block(toKey))
    val undoKey = math.min(fromKey,toKey)

    block(undoKey) ++= block(math.max(fromKey,toKey))
    for (v <- block(math.max(fromKey,toKey))) {
      partition(v) = undoKey
    }
    block.remove(math.max(fromKey,toKey))

    println("heyho!")
    for ((ktmp,tmp) <- block) {
      for ((key, value) <- block) {
        if (!hasGrtEdges(dg, tmp, value)) {
          if (hasSomePathWithGrt(dg, tmp, value) || hasSomePathWithGrt(dg, value, tmp)) return true
        }
      }
    }
    undoMerge(undo, undoKey)
    false
  }

  def undoMerge(undo: mutable.HashMap[Int, Set[Set[ExtendedAtom]]], undoKey: Int) = {
    block.remove(undoKey)
    block ++= undo
    for((key,value) <- undo){
      for(v <- value){
        partition(v) = key
      }
    }
  }

  def hasGrtEdges(dg: DiGraph[Set[ExtendedAtom]], from: Set[Set[ExtendedAtom]], to: Set[Set[ExtendedAtom]]): Boolean = {
    for ((f,t) <- grtEdges) {
      if (from.contains(f)) {
        if (to.contains(t)) return true
      }
    }
    false
  }

  /* checks if a node has already been added to block */
  def inBlock(node: Set[ExtendedAtom]) : Boolean = {
      if (partition.contains(node)) return true
    false
  }

  def isGrt(/*g: DepGraph, dg: DiGraph[Set[ExtendedAtom]], */from: Set[ExtendedAtom], to: Set[ExtendedAtom]): Boolean = {
    if(grtEdges.contains(from) && grtEdges(from) == to) return true
    false
  }

  /* @return true if there is a path with a `grt` edge along the way, false otherwise */
  def hasSomePathWithGrt(dg: DiGraph[Set[ExtendedAtom]], from: Set[Set[ExtendedAtom]], to: Set[Set[ExtendedAtom]]): Boolean = {
    val marked = new collection.mutable.HashMap[Set[ExtendedAtom], Boolean]()
    for (n <- dg.nodes) {
      marked(n) = false
    }
    for (v1 <- from) {
      for (v2 <- to) {
        if (hasPathWithGrt(false, dg, v1, v2, marked) == (true,true)) {
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
  def hasPathWithGrt(foundGrtBefore: Boolean, dg: DiGraph[Set[ExtendedAtom]], v1: Set[ExtendedAtom], v2: Set[ExtendedAtom], marked: mutable.HashMap[Set[ExtendedAtom], Boolean]): (Boolean,Boolean) = {
    marked(v1) = true
    for (w <-dg.outgoing(v1)) {
      if (!marked(w)) {
        val foundGreater = foundGrtBefore || isGrt(v1, w)
        if (w == v2) { //reached target
          return (foundGreater,true)
        }
        val foundGrtPath = hasPathWithGrt(foundGreater,dg, w, v2, markedCopy(marked))
        if (foundGrtPath._1 && foundGrtPath._2) {
          return foundGrtPath
        }
      }
    }
    (false,false)
  }

  /* creates a copy for every call of the check method, so all the paths get visited */
  def markedCopy(marked: mutable.HashMap[Set[ExtendedAtom], Boolean]): mutable.HashMap[Set[ExtendedAtom], Boolean] = {
    val result = new mutable.HashMap[Set[ExtendedAtom],Boolean]()
    for ((key,value) <- marked) {
      result += (key -> value)
    }
    result
  }
}
