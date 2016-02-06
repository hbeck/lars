package jtms.tmn.examples

import jtms.out
import jtms.tmn.TMNSpec

/**
  * Created by FM on 05.02.16.
  */
class JTMS_4 extends JTMS {

  val tmn = {
    val tmn = JTMS
    tmn.set(Set(E, B, D).to)
    tmn
  }

  "Node A" should "have Status out" in {
    assert(tmn.status(A) == out)
  }
  it should "have Justifications j1" in {
    assert(tmn.SJ(A).isEmpty)
  }
  it should "have Supp C" in {
    assert(tmn.Supp(A) == Set(C))
  }
  it should "have Supp* C,A" in{
    assert(tmn.SuppTrans(A) == Set(C,A))
  }
  it should "have Cons B,C" in {
    assert(tmn.Cons(A) == Set(B, C))
  }
  it should "have ACons B,C" in {
    assert(tmn.ACons(A) == Set(B, C))
  }
  it should "have ACons* B,C,D,A,F" in {
    assert(tmn.AConsTrans(A) == Set(B, C, D, A, F))
  }
  
  "With D" should "support only B" in {
    assert(tmn.Supp(D) == Set(B))
  }

  "With B" should "support only A" in {
    assert(tmn.Supp(B) == Set(A))
  }


  "The support of F" should "contain only C" in {
    assert(tmn.Supp(F) == Set(C))
  }

}
