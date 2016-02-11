package jtms.tmn.examples

import jtms.{Premise, TMN, Justification, Node}
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
class Car extends FlatSpec {

  val S_not = new Node("car is not starting")
  val G_not = new Node("not enough gas")
  val G = new Node("enough gas")
  val D = new Node("defect")
  val I = new Node("ignition broken")
  val C = new Node("carb broken")

  val N_cont = new Node("contradiction")

  val j0 = Justification.in(S_not).out(D).node(G_not);
  val j1 = Justification.in(S_not, G).node(D)
  val j2 = Justification.in(G, G_not).node(N_cont)
  val j3 = Justification.in(I).node(D)
  val j4 = Justification.in(C).node(D)

  val notStarting = Premise(S_not)
  val enoughGas = Premise(G)
  val notEnoughGas = Premise(G_not)
  val brokenIgnition = Premise(C)

  def TMN = {
    val tmn = new TMN(Set(S_not, G, G_not, D, I, C, N_cont));
    tmn.Ncont.add(N_cont)

    tmn.update(j0)
    tmn.update(j1)
    tmn.update(j2)
    tmn.update(j3)
    tmn.update(j4)

    tmn
  }

  "When the car is not starting and there is not enough gas" should "not result in a defect" in {
    val tmn = TMN

    tmn.update(notStarting)
    tmn.update(notEnoughGas)

    val model = tmn.getModel()

    assert(model.contains(D) == false)
  }

  "When the car is not starting and there is enough gas" should "result in a defect" in {
    val tmn = TMN

    tmn.update(notStarting)
    tmn.update(enoughGas)

    val model = tmn.getModel()

    assert(model.contains(D))
  }

  "When the car is not starting and there is no gas information" should "result in not enough gas" in {
    val tmn = TMN

    tmn.update(notStarting)

    val model = tmn.getModel()

    assert(model.contains(G_not))
  }

  "When the car is not starting and there is a broken ignition" should "result in a defect" in {
    val tmn = TMN

    tmn.update(notStarting)
    tmn.update(brokenIgnition)

    val model = tmn.getModel()

    assert(model.contains(D))
  }

  "When the car is not starting and there is a broken ignition and enogh gas" should "result in a defect" in {
    val tmn = TMN

    tmn.update(notStarting)
    tmn.update(enoughGas)
    tmn.update(brokenIgnition)

    val model = tmn.getModel()

    assert(model.contains(D))
  }
}
