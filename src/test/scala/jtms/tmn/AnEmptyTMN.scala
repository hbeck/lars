package jtms.tmn

import jtms.{in, Justification, TMN, Node}
import org.scalatest.{FlatSpec}

/**
  * Created by FM on 05.02.16.
  */
class AnEmptyTMN extends FlatSpec {

  def Assumption(node: String): Justification = Assumption(new Node(node))

  def Assumption(node: Node): Justification = new Justification(Set(), Set(), node)

  def EmptyTMN = new TMN(Set())

  val assumptionA = Assumption("A")

  def evaluate = {
    val emptyTMN =EmptyTMN

    emptyTMN.update(assumptionA)

    emptyTMN
  }

  "An empty TMN" should "generate a status entry after update" in {
    assert(evaluate.status.isEmpty == false)
  }

  it should "contain one entry with status in" in {
    assert(evaluate.status.filter(_._2  == in).size == 1)
  }

}
