import lars.core.semantics.formulas.WindowOperators.ch2
import lars.core.semantics.formulas.{Formula, W, At, Atom}
import lars.core.windowfn.timebased.{TimeBasedWindowParameters, TimeBasedWindow}
val h = Atom("gc","Id1","Id2","X") //TODO variable object?
//
var t:Int = 0 // TODO
val wt = TimeBasedWindow
val prm = TimeBasedWindowParameters(0,5,1)
//
val b1 = At(t,Atom("exp","Id1","X"))
val b2 = At(t,W(wt,ch2,prm,Atom("exp","Id2","X")))
//val b3 = Neq("Id1","Id2")
val b3 = Atom("old","Id2")
//
case class Rule(head:Formula,posBody:Set[Formula],negBody:Set[Formula]) {
  def this(head:Formula,posBody:Set[Formula]) = this(head,posBody,Set[Formula]())
  def this(head:Formula,posBody:Formula) = this(head,Set[Formula](posBody),Set[Formula]())
}
case class Program(rules:Set[Rule])