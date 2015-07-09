import lars.core.semantics.formulas.WindowOperators.ch2
import lars.core.semantics.formulas._
import lars.core.windowfn.time.{TimeWindowParameters, TimeWindow}
case class gc(t1:Term, t2: Term, t3: Term) extends Atom
case class exp(t1:Term, t2: Term) extends Atom
case class old(t:Term) extends Atom
val h = gc("Id1","Id2","X") //TODO variable object?
//
var t:Int = 0 // TODO
val wt = TimeWindow
val prm = TimeWindowParameters(0,5,1)
//
val b1 = At(t,exp("Id1","X"))
val b2 = At(t,W(WindowOperator(wt,ch2,prm),exp("Id2","X")))
//val b3 = Neq("Id1","Id2")
val b3 = old("Id2")
//
case class Rule(head:Formula,posBody:Set[Formula],negBody:Set[Formula]) {
  def this(head:Formula,posBody:Set[Formula]) = this(head,posBody,Set[Formula]())
  def this(head:Formula,posBody:Formula) = this(head,Set[Formula](posBody),Set[Formula]())
}
case class Program(rules:Set[Rule])