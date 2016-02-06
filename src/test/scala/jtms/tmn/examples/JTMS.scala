package jtms.tmn.examples

import jtms.{TMN, Node, Justification}
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16.
  */
class JTMS extends FlatSpec {

  val A = Node("A")
  val B = Node("B")
  val C = Node("C")
  val D = Node("D")
  val E = Node("E")
  val F = Node("F")

  val j1 = new Justification(Set(C), Set(), A)
  val j2 = new Justification(Set(), Set(A), B)
  val j3 = new Justification(Set(A), Set(), C)
  val j4a = new Justification(Set(B), Set(), D)
  val j4b = new Justification(Set(C), Set(), D)
  val j5 = new Justification(Set(), Set(), E)
  val j6 = new Justification(Set(C, E), Set(), F)

  def JTMS = {
    //    var tmn = new TMN(Set(A, B, C, D, E, F), Set(j1, j2, j3, j4a, j4b, j5, j6).to)

    val tmn = new TMN(Set(A, B, C, D, E, F))
    tmn.update(j1)
    tmn.update(j2)
    tmn.update(j3)
    tmn.update(j4a)
    tmn.update(j4b)
    tmn.update(j5)
    tmn.update(j6)

    tmn
  }
}
