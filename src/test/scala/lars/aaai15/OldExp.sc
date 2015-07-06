import lars.core.semantics.formulas._
import FormulaFunctions._
import WindowOperators._
import lars.core.semantics.streams.{S, Evaluation, Timeline}
import lars.core.semantics.structure.M

import lars.core.windowfn.timebased.{TimeBasedWindowParameters, TimeBasedWindowParam}
import scala.collection.immutable.{HashMap, HashSet}
//
case class tram(vehicleId:Term, station: Term) extends Atom
case class exp(vehicleId:Term, station: Term) extends Atom
//
val T = Timeline(0,50)
val a36 = tram("a1","b")
val a40 = tram("a3","h")
val v = Evaluation(HashMap(36 -> Set(a36),40 -> Set(a40)))
val D = S(T,v) //Ex 2
//
"w(D,42,(4,0,1))" // Ex 3
val params0 = TimeBasedWindowParameters(4,0,1)
val Sw = TimeBasedWindowParam apply (D,42,params0)
val Sw2 = TimeBasedWindowParam apply (D,42,4,0,1) //alternative input
val Sw3 = TimeBasedWindowParam apply (D,42,4) //=> D,42,4,0,1
//
val T0 = T
val a43 = exp("a3","m")
val a44 = exp("a1","m")
val v0 = Evaluation(v.mapping + (43 -> Set[Atom](a43),44 -> Set[Atom](a44)))
//
"S*"
val s = S(T0,v0)
val B = new HashSet[Atom]
val m = M(T0,v0,B)
//
"M,S*,43 ||- exp(a3,m) ?"
m/s/43 ||- a43
m/s/42 ||- diamond(a43)
m/s/42 ||- not(box(a43))
m/s/42 ||- (diamond(a43) or box(a43))
m/s/42 ||- (diamond(a43) and box(a43))
//
"M,S*,42 ||- @43 exp(a3,m) ?"
m/s/42 ||- At(43,a43)
//
"M,S*,42 ||- @43 exp(a3,m) ?"
m/s/42 ||- at(43) (a43)
val tau = TimeBasedWindowParam
val params1 = TimeBasedWindowParameters(0,5,1)
val w_5 = gwin(tau,ch2,params1)
//
"M,S*,42 ||- w+5 d exp(a3,m) ?"
m/s/42 ||- W(WindowOperator(tau,ch2,params1),diamond(a43)) //true
//
"M,S*,42 ||- w+5 d exp(a3,m) ?"
m/s/42 ||- w_5 (diamond(a43)) //true
//
"M,S*,10 ||- w+5 d exp(a3,m) ?"
m/s/10 ||- w_5 (diamond(a43)) //false
//
"M,S*,10 ||- (w+5 d exp(a3,m)) -> B(tram(a1,b)) and -(D(tram(a1,b)))?"
m/s/10 ||- ((w_5 (diamond(a43))) implies (box(a36) and not(diamond(a36)))) //true
m/10 |= ((w_5 (diamond(a43))) implies (box(a36) and not(diamond(a36)))) //true