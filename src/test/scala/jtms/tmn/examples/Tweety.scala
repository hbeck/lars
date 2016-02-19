package jtms.tmn.examples

import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
class Tweety extends FlatSpec {

  val V = Node("Vogel sein")
  val P = Node("Pinguin sein")
  val F = Node("Fliegen können")
  val F_not = Node("nicht fliegen können")
  val N_cont = ContradictionNode("Widerspruch")

  val j0 = Justification.in(P).node(F_not)
  val j1 = Justification.in(P).node(V)
  val j2 = Justification.in(V).out(P).node(F)
  val j3 = Justification.in(F, F_not).node(N_cont)
  val j4 = Premise(V)

  val j5 = Premise(P)

  def TMN = {
    val tmn = new TMN(Set(V, P, F, F_not, N_cont))

    tmn.update(j0)
    tmn.update(j1)
    tmn.update(j2)
    tmn.update(j3)
    tmn.update(j4)

    tmn
  }

  "The initial model" should "contain only V and F" in {
    assert(TMN.getModel() == Set(V, F))
  }

  "Adding a new Premise P" should "result in a new Model containing V, P and F_not" in {
    val tmn = TMN

    tmn.update(j5)

    assert(tmn.getModel() == Set(V, P, F_not))
  }
}
