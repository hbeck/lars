package jtms


class JustificationBuilder(I: Set[Node] = Set(), O: Set[Node] = Set()) {
  def in(nodes: Node*) = new JustificationBuilder(I ++ nodes, O)

  def out(nodes: Node*) = new JustificationBuilder(I, O ++ nodes)

  def node(n: Node) = new Justification(I, O, n)
}

object Premise {
  def apply(n: Node) = Justification.premise(n)
}

object Justification {
  def in(nodes: Node*) = new JustificationBuilder(nodes.toSet)

  def out(nodes: Node*) = new JustificationBuilder(Set(), nodes.toSet)

  def premise(n: Node) = new Justification(Set(), Set(), n)
}

/**
  * Created by hb on 12/22/15.
  */
case class Justification(I: Set[Node], O: Set[Node], n: Node)