package lars.graph

/**
 * Created by hb on 7/17/15.
 */
class LabeledDiGraph[V,L](override val adjList:Map[V,Set[V]], val label: ((V,V) => L)) extends DiGraph[V](adjList) {

  def ==(other: LabeledDiGraph[V,L]): Boolean = {
    if (adjList != other.adjList) {
      return false
    }
    for (k <- adjList.keys) {
      for (v <- adjList(k)) {
        if (label(k,v) != other.label(k,v)) {
          return false
        }
      }
    }
    true
  }

  def !=(other: LabeledDiGraph[V,L]): Boolean = {
    !(this == other)
  }

  override def equals(other:Any) : Boolean = {
    other match {
      case g: LabeledDiGraph[V,L] => this == g
      case _ => false
    }
  }

}
