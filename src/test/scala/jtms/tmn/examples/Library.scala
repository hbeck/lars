package jtms.tmn.examples

import jtms._
import jtms.tmn.NodeValidation
import org.scalatest.{GivenWhenThen, FlatSpec}

/**
  * Created by FM on 11.02.16.
  */
class Library extends FlatSpec with NodeValidation with GivenWhenThen {

  val V = new Node("verfügbar")
  val G = new Node("gestohlen")
  val P = new Node("am angegebenen Platz vorhanden")
  val F = new Node("falsch einsortiert")
  val P_not = new Node("nicht am angegebenen Platz vorhanden")
  val A = new Node("ausleihbar")
  val N = new Node("Nachschlagewerk")
  val A_not = new Node("nicht ausleihbar")
  val H = new Node("im Handapperart einer Veranstaltung")
  val N_cont = new Node("Widerspruch")

  val j1 = Premise(V)
  val j2 = Justification.in(V).out(F, G).node(P)
  val j3 = Justification.in(F).node(P_not)
  val j4 = Justification.in(G).node(P_not)
  val j5 = Justification.in(P).out(H, N).node(A)
  val j6 = Justification.in(P, P_not).node(N_cont)
  val j7 = Justification.in(N).node(A_not)
  val j8 = Justification.in(H).node(A_not)
  val j9 = Justification.in(A, A_not).node(N_cont)

  def TMN = {
    val tmn = new TMN(Set(V, G, P, F, P_not, A, N, A_not, H, N_cont))

    tmn.Ncont.add(N_cont)

    tmn.update(j1)
    tmn.update(j2)
    tmn.update(j3)
    tmn.update(j4)
    tmn.update(j5)
    tmn.update(j6)
    tmn.update(j7)
    tmn.update(j8)
    tmn.update(j9)

    tmn
  }

  "The valid model" should "be V, P, A" in {
    assert(TMN.getModel() == Set(V, P, A))
  }

  "Node V" must behave like nodeValidation(TMN, V) { validator =>
    validator.state(in)
    validator.justifications(j1)
    validator.SJ(Some(j1))
    validator.Supp()
    validator.SuppTrans()
    validator.Ant()
    validator.AntTrans()
    validator.Cons(P)
    validator.ACons(P)
    validator.AConsTrans(P, A)
  }

  "Node P" must behave like nodeValidation(TMN, P) { validator =>
    validator.state(in)
    validator.justifications(j2)
    validator.SJ(Some(j2))
    validator.Supp(V, F, G)
    validator.SuppTrans(V, F, G)
    validator.Ant(V, F, G)
    validator.AntTrans(V, F, G)
    validator.Cons(A, N_cont)
    validator.ACons(A)
    validator.AConsTrans(A)
  }

  "Node A" must behave like nodeValidation(TMN, A) { validator =>
    validator.state(in)
    validator.justifications(j5)
    validator.SJ(Some(j5))
    validator.Supp(P, H, N)
    validator.SuppTrans(P, H, N, V, F, G)
    validator.Ant(P, H, N)
    validator.AntTrans(P, H, N, V, F, G)
    validator.Cons(N_cont)
    validator.ACons()
    validator.AConsTrans()
  }

  "Node F" must behave like nodeValidation(TMN, F) { validator =>
    validator.state(out)
    validator.justifications()
    validator.SJ(None)
    validator.Supp()
    validator.SuppTrans()
    validator.Ant()
    validator.AntTrans()
    validator.Cons(P, P_not)
    validator.ACons(P, P_not)
    validator.AConsTrans(P, A, P_not, N_cont)
  }
  "Node G" must behave like nodeValidation(TMN, G) { validator =>
    validator.state(out)
    validator.justifications()
    validator.SJ(None)
    validator.Supp()
    validator.SuppTrans()
    validator.Ant()
    validator.AntTrans()
    validator.Cons(P, P_not)
    validator.ACons(P, P_not)
    validator.AConsTrans(P, A, P_not, N_cont)
  }


  "Node H" must behave like nodeValidation(TMN, H) { validator =>
    validator.state(out)
    validator.justifications()
    validator.SJ(None)
    validator.Supp()
    validator.SuppTrans()
    validator.Ant()
    validator.AntTrans()
    validator.Cons(A, A_not)
    validator.ACons(A, A_not)
    validator.AConsTrans(A, A_not, N_cont)
  }
  "Node N" must behave like nodeValidation(TMN, N) { validator =>
    validator.state(out)
    validator.justifications()
    validator.SJ(None)
    validator.Supp()
    validator.SuppTrans()
    validator.Ant()
    validator.AntTrans()
    validator.Cons(A, A_not)
    validator.ACons(A, A_not)
    validator.AConsTrans(A, A_not, N_cont)
  }

  "Node P_not" must behave like nodeValidation(TMN, P_not) { validator =>
    validator.state(out)
    validator.SJ(None)
    validator.justifications(j3, j4)
    validator.Supp(F, G)
    validator.SuppTrans(F, G)
    validator.Ant()
    validator.AntTrans()
    validator.Cons(N_cont)
    validator.ACons(N_cont)
    validator.AConsTrans(N_cont)
  }

  "Node A_not" must behave like nodeValidation(TMN, A_not) { validator =>
    validator.state(out)
    validator.SJ(None)
    validator.justifications(j8, j7)
    validator.Supp(H, N)
    validator.SuppTrans(H, N)
    validator.Ant()
    validator.AntTrans()
    validator.Cons(N_cont)
    validator.ACons(N_cont)
    validator.AConsTrans(N_cont)
  }

  "Node N_cont" must behave like nodeValidation(TMN, N_cont) { validator =>
    validator.state(out)
    validator.SJ(None)
    validator.justifications(j6, j9)
    validator.Supp(P_not, A_not)
    validator.SuppTrans(P_not, A_not, F, G, H, N)
    validator.Ant()
    validator.AntTrans()
    validator.Cons()
    validator.ACons()
    validator.AConsTrans()
  }

  "With the premise H the model" should "be V,H,P,A_not" in {
    val tmn = TMN
    tmn.update(Premise(H))

    val model = tmn.getModel()

    assert(model == Set(V, H, P, A_not))
  }

  "With a contradiction node for A the model" should "be A_not,H,P, V" in {
    val tmn = TMN

    val N_cont2 = new Node("Widerspruch2")
    tmn.Ncont.add(N_cont2)

    tmn.update(Justification.in(A).node(N_cont2))

    val model = tmn.getModel()
    assert(model == Set(A_not, H, P, V))
  }
  it should "also return the same model when using just a single contradiction node" in {
    val tmn = TMN

    tmn.update(Justification.in(A).node(N_cont))

    val model = tmn.getModel()

    assert(model == Set(A_not, H, P, V))
  }
}