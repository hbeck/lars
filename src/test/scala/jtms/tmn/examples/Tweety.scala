package jtms.tmn.examples

import jtms.{TMN, Justification, Node}
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
class Tweety extends FlatSpec {

  val V = new Node("Vogel sein")
  val P = new Node("Pinguin sein")
  val F = new Node("Fliegen können")
  val F_not = new Node("nicht fliegen können")
  val N_cont = new Node("Widerspruch")

  val j0 = new Justification(Set(P), Set(), F_not)
  val j1 = new Justification(Set(P), Set(), V)
  val j2 = new Justification(Set(V), Set(P), F)
  val j3 = new Justification(Set(F, F_not), Set(), N_cont)
  val j4 = new Justification(Set(), Set(), V)

  val j5 = new Justification(Set(), Set(), P)

  def TMN = {
    val tmn = new TMN(Set(V, P, F, F_not, N_cont))

    tmn.Ncont.add(N_cont)

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
