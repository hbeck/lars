package jtms.tmn.examples

import jtms.{Premise, TMN, Justification, Node}
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
class Manufacturing extends FlatSpec {

  val C = new Node("Product")
  val B = new Node("troubles")
  val A1 = new Node("Resource 1")
  val A2 = new Node("Resource 2")
  val L1 = new Node("supply problems A1")

  val j0 = Justification.in(C).out(B).node(A1)
  val j1 = Justification.in(C, B).node(A2)
  val j2 = Justification.in(L1).node(B)
  val j3 = Justification.premise(C)

  def TMN = {
    val tmn = new TMN(Set(C, B, A1, A2, L1))

    tmn.update(j0)
    tmn.update(j1)
    tmn.update(j2)
    tmn.update(j3)

    tmn
  }

  "When manufacturing without troubles" should "use resource A1" in {
    val tmn = TMN;

    assert(tmn.getModel() == Set(C, A1))
  }

  "When there are supply problems with A1" should "mark as troubles and use resource A2" in {
    val tmn = TMN;

    tmn.update(Premise(L1))

    assert(tmn.getModel() == Set(C, L1, B, A2))
  }

}
