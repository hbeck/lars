package lars.exp

import lars.exp.Formulas._
import lars.exp.Streams._

import scala.collection.immutable.{HashMap, HashSet, Set}

/**
 * Created by hb on 1/2/15.
 */
object Experiments {
  def main(args: Array[String]) {

    val T = Timeline(0,50)
    val a36 = Atom("tram(a1,b)")
    val a40 = Atom("tram(a3,h)")
    val v36 = 36 -> Set(a36)
    val v40 = 40 -> Set(a40)
    val v = Evaluation(HashMap(v36,v40))
    val D = Stream(T,v)

    println(D)

    val T0 = T
    val a43 = Atom("exp(a3,m)")
    val a44 = Atom("exp(a1,m)")
    val v43 = 43 -> Set(a43)
    val v44 = 44 -> Set(a44)
    val v0 = Evaluation(v.map + (v43,v44))
    val S0 = Stream(T0,v0)

    println(S0)

    val W = new HashSet[WindowOperator]
    val B = new HashSet[Atom]
    val M = Structure(T0,v0,W,B)

    val q1 = M/S0/43 ||- a43

    println("M,S*,43 ||- exp(a3,m) ?: "+q1)

  }
}


