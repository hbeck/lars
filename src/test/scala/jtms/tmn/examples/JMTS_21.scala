package jtms.tmn.examples

import jtms.{UserDefinedNode, Justification, Node}

/**
  * Created by FM on 11.02.16.
  */
class JMTS_21 extends JTMS {
  val N_cont = Node("N_cont")

  val j7 = Justification.in(B).out(C).node(N_cont)

  def JTMS_DDB = {
    val tmn = JTMS

    tmn.Ncont.add(N_cont)

    tmn.update(j7)

    tmn
  }

  "The model" should "contain A,C,D,F,E" in {
    assert(JTMS_DDB.getModel() == Set(A, C, D, F, E))
  }
}
