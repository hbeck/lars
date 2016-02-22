package jtms.tmn.examples

import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
class Car extends FlatSpec {

  val S_not = Node("car is not starting")
  val G_not = Node("not enough gas")
  val G = Node("enough gas")
  val D = Node("defect")
  val I = Node("ignition broken")
  val C = Node("carb broken")

  val N_cont = ContradictionNode("contradiction")

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

    tmn.add(j0)
    tmn.add(j1)
    tmn.add(j2)
    tmn.add(j3)
    tmn.add(j4)

    tmn
  }

  "When the car is not starting and there is not enough gas" should "not result in a defect" in {
    val tmn = TMN

    tmn.add(notStarting)
    tmn.add(notEnoughGas)

    val model = tmn.getModel()

    assert(model.contains(D) == false)
  }

  "When the car is not starting and there is enough gas" should "result in a defect" in {
    val tmn = TMN

    tmn.add(notStarting)
    tmn.add(enoughGas)

    val model = tmn.getModel()

    assert(model.contains(D))
  }

  "When the car is not starting and there is no gas information" should "result in not enough gas" in {
    val tmn = TMN

    tmn.add(notStarting)

    val model = tmn.getModel()

    assert(model.contains(G_not))
  }

  "When the car is not starting and there is a broken ignition" should "result in a defect" in {
    val tmn = TMN

    tmn.add(notStarting)
    tmn.add(brokenIgnition)

    val model = tmn.getModel()

    assert(model.contains(D))
  }

  "When the car is not starting and there is a broken ignition and enogh gas" should "result in a defect" in {
    val tmn = TMN

    tmn.add(notStarting)
    tmn.add(enoughGas)
    tmn.add(brokenIgnition)

    val model = tmn.getModel()

    assert(model.contains(D))
  }
}
