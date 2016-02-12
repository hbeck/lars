package jtms.tmn.examples

import jtms.{TMN, Justification, Node}
import org.scalatest.FlatSpec

/**
  * Created by FM on 12.02.16.
  */
class MultipleModels  extends  FlatSpec{

  val A = new Node("A")
  val B = new Node("B")

  val j1 = Justification.out(A).node(B)
  val j2 = Justification.out(B).node(A)

  def TMN = new TMN(Set(A,B))

  "When adding j1 before j2 the valid model" should "be B" in{
    val tmn = TMN

    tmn.update(j1)
    tmn.update(j2)

    assert(tmn.getModel() == Set(B))
    pending
  }

  "When adding j2 before j1 the valid model" should "be A" in{
    val tmn = TMN

    tmn.update(j2)
    tmn.update(j1)

    assert(tmn.getModel() == Set(A))
    pending
  }
}
