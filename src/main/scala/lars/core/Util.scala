package lars.core

import lars.core.semantics.formulas.{Atom, C, V, Term}
import lars.core.semantics.streams.{Evaluation, S}
import scala.language.implicitConversions

/**
 * Created by hb on 5/19/15.
 */
object Util {

  implicit def str2Term(s: String): Term = {
    if (s.charAt(0).isLower) C(s) else V(s)
  }

  //TODO revisit
  def merge[K,V](m1: Map[K,Set[V]], m2: Map[K,Set[V]]) = {
    val keys:Set[K] = m1.keySet.union(m2.keySet)
    def mergedSet(k:K) = {
      val v1: Set[V] = m1.applyOrElse(k,{k:K=>Set[V]()})
      val v2: Set[V] = m2.applyOrElse(k,{k:K=>Set[V]()})
      v1 union v2
    }
    var m = Map[K,Set[V]]()
    for (k <- keys) {
      val v = mergedSet(k)
      m = m + ((k,v))
    }
    m
  }

  def iterateProperSubstreams(s:S): Iterator[S] = {

    //from all mappings k -> {v1, ..., vn} create a set
    //k -> v1, ..., k -> vn;
    //create a set of all these single-atom mappings (for all keys)
    var tsAtoms:Set[(Int,Atom)] = Set()
    for (k <- s.v.mapping.keys) {
      val tsAtomsK = s.getTimestampedAtoms()
      tsAtoms ++= tsAtomsK
    }
    val timestampedAtoms = tsAtoms.toSet

    //iterate over the power set, create stream
    var properSubstreams = Set[S]()
    for (subset <- timestampedAtoms.subsets()) {
      if (subset != timestampedAtoms) {
        //properSubstreams += S.fromTimestampedAtoms(s.T,subset)
        properSubstreams += S(s.T, Evaluation.fromTimestampedAtoms(subset))
      }
    }
    properSubstreams.iterator //TODO proper (lazy) iterator
  }

  //TODO extract all map functions with maps of type [X,Set[Y]]

}
