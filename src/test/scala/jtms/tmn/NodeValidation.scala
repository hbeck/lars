package jtms.tmn

import jtms.{Justification, Status, Node, TMN}
import org.scalatest.{GivenWhenThen, FlatSpec}

/**
  * Created by FM on 11.02.16.
  */

trait NodeValidation {
  this: FlatSpec with GivenWhenThen =>

  def nodeValidation(tmn: TMN, n: Node): ((NodeValidator) => Any) => Any = {
    val nc = new NodeValidator(tmn, n)

    def nodeCheckTestCallback(nodeCheck: (NodeValidator) => Any) = {
      nodeCheck(nc)
    }

    return nodeCheckTestCallback
  }

  class NodeValidator(tmn: TMN, n: Node) {

    Given(n.toString)

    def state(status: Status) = {
      Then("the state is " + status)

      assert(tmn.status(n) == status)
    }

    def justifications(justifications: Justification*) = {
      val justificationSet = justifications.toSet
      Then("the justifications are " + justificationSet)

      assert(tmn.Jn(n) == justificationSet)
    }

    def SJ(j: Option[Justification]) = {
      if (j.isDefined)
        Then("the supporting justification is " + j)
      else
        Then("there are no supporting justifications")

      assert(tmn.SJ(n) == j)
    }

    def Supp(nodes: Node*) = {
      Then("the Supp is " + nodes.toSet)
      assert(tmn.Supp(n) == nodes.toSet)
    }

    def SuppTrans(nodes: Node*) = {
      Then("the Supp* is " + nodes.toSet)
      assert(tmn.SuppTrans(n) == nodes.toSet)
    }

    def Ant(nodes: Node*) = {
      Then("the Ant is " + nodes.toSet)
      assert(tmn.Ant(n) == nodes.toSet)
    }

    def AntTrans(nodes: Node*) = {
      Then("the Ant* is " + nodes.toSet)
      assert(tmn.AntTrans(n) == nodes.toSet)
    }

    def Cons(nodes: Node*) = {
      Then("the Cons is " + nodes.toSet)
      assert(tmn.Cons(n) == nodes.toSet)
    }

    def ACons(nodes: Node*) = {
      Then("the ACons is " + nodes.toSet)
      assert(tmn.ACons(n) == nodes.toSet)
    }

    def AConsTrans(nodes: Node*) = {
      Then("the ACons* is " + nodes.toSet)
      assert(tmn.AConsTrans(n) == nodes.toSet)
    }
  }

}

