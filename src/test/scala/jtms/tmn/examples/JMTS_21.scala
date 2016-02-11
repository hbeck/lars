package jtms.tmn.examples

import jtms.{Justification, Node}

/**
  * Created by FM on 11.02.16.
  */
class JMTS_21 extends JTMS {
  val N_cont = new Node("N_cont")

  val j7 = new Justification(Set(B), Set(C), N_cont)

  def JTMS_DDB = {
    val tmn = JTMS

    tmn.Ncont.add(N_cont)

    tmn.update(j7)

    tmn
  }

  "The MaxAssumptions for the contradiction node" should "contain Justification j2" in {
    val tmn = JTMS_DDB
    assert(tmn.MaxAssumptions(N_cont) == Set(j2))
  }


  "The model" should "contain A,C,D,F,E" in {
    assert(JTMS_DDB.getModel() == Set(A, C, D, F, E))
  }
}
