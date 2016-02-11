package jtms.tmn

import jtms.{Premise, TMN, Node, Justification}
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16.
  */
class TMNSpec extends FlatSpec {
  def Assumption(node: String): Justification = Assumption(new Node(node))

  def Assumption(node: Node): Justification = Premise(node)

  def EmptyTMN = new TMN(Set())
}
