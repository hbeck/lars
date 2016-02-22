package jtms.tmn

import jtms.tmn.examples.{Library, Tweety, JTMS_5, JMTS_21}
import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 12.02.16.
  */
class Deletion extends FlatSpec {

  val A = Node("A")
  val B = Node("B")
  val C = Node("C")

  "A model with only one justification" should "have no Justifications and Nodes after deletion" in {
    val j0 = Premise(A)

    val tmn = new TMN(Set(A))
    tmn.add(j0)

    assume(tmn.getModel() == Set(A))

    tmn.remove(j0)

    // TODO: Define what needs to be reset by remove/delete
    //    assert(tmn.N.isEmpty)
    //    assert(tmn.status.isEmpty)

    assert(tmn.J.isEmpty)
    assert(tmn.getModel() == Set())
  }

  "A stable TMN with 2 Nodes and two justifications" should "have an empty model after deletion of a supporting Premise" in {
    // arrange
    val j0 = Justification.in(A).node(B)
    val j1 = Premise(A)

    val tmn = new TMN(Set(A, B))

    tmn.add(j0)
    tmn.add(j1)

    assume(tmn.getModel() == Set(A, B))

    // act
    tmn.remove(j1)

    // assert
    assert(tmn.getModel().isEmpty)

    assert(tmn.J == Set(j0))
    assert(tmn.Supp(A).isEmpty)
    assert(tmn.SJ(A) == None)
    assert(tmn.SJ(B) == None)
    assert(tmn.Cons(A) == Set(B))
  }

  it should "have the Model A after deletion of a justification" in {
    // arrange
    val j0 = Justification.in(A).node(B)
    val j1 = Premise(A)

    val tmn = new TMN(Set(A, B))

    tmn.add(j0)
    tmn.add(j1)

    assume(tmn.getModel() == Set(A, B))

    // act
    tmn.remove(j0)

    // assert
    assert(tmn.getModel() == Set(A))

    assert(tmn.J == Set(j1))
    assert(tmn.Supp(A) == Set())
    assert(tmn.SJ(A) == Some(j1))
    assert(tmn.Cons(A) == Set())
  }

  "A TMN with three nodes" should "have only Model A after deleting a justification" in {
    val j0 = Justification.in(A).node(B)
    val j1 = Premise(A)
    val j2 = Justification.in(B).node(C)

    val tmn = new TMN(Set(A, B, C))

    tmn.add(j0)
    tmn.add(j1)
    tmn.add(j2)

    assume(tmn.getModel() == Set(A, B, C))

    tmn.remove(j0)

    assert(tmn.getModel() == Set(A))

    assume(tmn.J == Set(j1, j2))
    assert(tmn.Supp(C) == Set(B))
    assert(tmn.Cons(A) == Set())
    assert(tmn.SJ(C) == None)
  }

  "A TMN with three nodes and a redundant justification" should "have Model A,C after deleting a justification supporting B" in {
    val j0 = Justification.in(A).node(B)
    val j1 = Premise(A)
    val j2 = Justification.in(B).node(C)
    val j3 = Justification.in(A).node(C)

    val tmn = new TMN(Set(A, B, C))

    tmn.add(j0)
    tmn.add(j1)
    tmn.add(j2)
    tmn.add(j3)

    assume(tmn.getModel() == Set(A, B, C))
    assume(tmn.SJ(C) == Some(j2))
    assume(tmn.Supp(C) == Set(B))
    assume(tmn.Cons(A) == Set(B, C))
    assume(tmn.Cons(B) == Set(C))

    tmn.remove(j0)

    assert(tmn.getModel() == Set(A, C))
    assert(tmn.J == Set(j1, j2, j3), "the SJ for C should change")
    info("the SJ for C should change")
    assert(tmn.SJ(C) == Some(j3))
    info("the Supp for C should change")
    assert(tmn.Supp(C) == Set(A))
    info("the Cons for A should change")
    assert(tmn.Cons(A) == Set(C))
    assert(tmn.Cons(B) == Set(C))
  }

  "Removing an additional justification form the JTMS 5 sample" should "result in the original model" in {
    // arrange
    val setup = new JTMS_5
    val tmn = setup.tmn

    assume(tmn.getModel() == Set(setup.A, setup.C, setup.D, setup.E, setup.F))

    // act
    tmn.remove(setup.j0)

    // assert
    assert(tmn.getModel() == Set(setup.E, setup.B, setup.D))
  }

  "Removing the Penguin premise from the Tweety sample" should "result in the Model V, F" in {
    // arrange
    val setup = new Tweety
    val tmn = setup.TMN

    tmn.add(setup.j5)

    assume(tmn.getModel() == Set(setup.F_not, setup.V, setup.P))

    // act
    tmn.remove(setup.j5)

    // assert
    assert(tmn.getModel() == Set(setup.V, setup.F))
  }

  "Removing a justification from a TMN where backtracking occurred" should "result in the original model" in {
    // arrange
    val setup = new JMTS_21
    val tmn = setup.JTMS_DDB

    assume(tmn.getModel() == Set(setup.A, setup.C, setup.D, setup.F, setup.E))

    // act
    tmn.remove(setup.j7)

    // assert
    assert(tmn.getModel() == setup.JTMS.getModel())
    assert(tmn.getModel() == Set(setup.E, setup.B, setup.D))
  }

  "Removing a exclusion justification for A in the library sample" should "result in the initial model" in {
    val setup = new Library
    val tmn = setup.TMN

    tmn.add(setup.jExclusionA)

    assume(tmn.getModel() == Set(setup.A_not, setup.H, setup.P, setup.V))

    // act
    tmn.remove(setup.jExclusionA)

    // assert
    assert(tmn.getModel() == Set(setup.A, setup.P, setup.V))
  }
}
