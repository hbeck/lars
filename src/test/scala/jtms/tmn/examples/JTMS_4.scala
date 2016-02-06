package jtms.tmn.examples

import jtms.{in, out}
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
  it should "have no Justifications" in {
    assert(tmn.SJ(A).isEmpty)
  }
  it should "have Supp C" in {
    assert(tmn.Supp(A) == Set(C))
  }
  it should "have Supp* C,A" in {
    assert(tmn.SuppTrans(A) == Set(C, A))
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

  "Node B" should "have Status in" in {
    assert(tmn.status(B) == in)
  }
  it should "have Justification j2" in {
    assert(tmn.SJ(B) == Some(j2))
  }
  it should "have Supp A" in {
    assert(tmn.Supp(B) == Set(A))
  }
  it should "have Supp* A,C" in {
    assert(tmn.SuppTrans(B) == Set(A, C))
  }
  it should "have Cons D" in {
    assert(tmn.Cons(B) == Set(D))
  }
  it should "have ACons D" in {
    assert(tmn.ACons(B) == Set(D))
  }
  it should "have ACons* D" in {
    assert(tmn.AConsTrans(B) == Set(D))
  }

  "Node C" should "have state out" in {
    assert(tmn.status(C) == out)
  }
  it should "have no Justification" in {
    assert(tmn.SJ(C).isEmpty)
  }
  it should "have Supp A" in {
    assert(tmn.Supp(C) == Set(A))
  }
  it should "have Supp* A,C" in {
    assert(tmn.SuppTrans(C) == Set(A, C))
  }
  it should "have Cons D" in {
    assert(tmn.Cons(C) == Set(A, D, F))
  }
  it should "have ACons D" in {
    assert(tmn.ACons(C) == Set(A, F))
  }
  it should "have ACons* A,F,B,C,D" in {
    assert(tmn.AConsTrans(C) == Set(A, F, B, C, D))
  }

  "Node D" should "have state in" in {
    assert(tmn.status(D) == in)
  }
  it should "have Justification j4a" in {
    assert(tmn.SJ(D) == Some(j4a))
  }
  it should "have Supp B" in {
    assert(tmn.Supp(D) == Set(B))
  }
  it should "have Supp* B,A,C" in {
    assert(tmn.SuppTrans(D) == Set(B, A, C))
  }
  it should "have no Cons" in{
    assert(tmn.Cons(D).isEmpty)
  }
  it should "have no ACons" in{
    assert(tmn.ACons(D).isEmpty)
  }
  it should "have no ACons*" in{
    assert(tmn.AConsTrans(D).isEmpty)
  }

  "The support of F" should "contain only C" in {
    assert(tmn.Supp(F) == Set(C))
  }

}
