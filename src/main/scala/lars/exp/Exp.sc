import lars.core.Formulas._
import lars.core.LStreams.{Evaluation, LStream, Timeline}
import lars.core.Structure
import lars.core.WindowOperators._
import lars.core.windowfn.{TimeBasedWindow, TimeBasedWindowParameters}
import scala.collection.immutable.{HashMap, HashSet}
//
val T = Timeline(0,50)
val a36 = Atom("tram(a1,b)")
val a40 = Atom("tram(a3,h)")
val v = Evaluation(HashMap(36 -> Set(a36),40 -> Set(a40)))
val D = LStream(T,v) //Ex 2
//
"w(D,42,(4,0,1))" // Ex 3
val params0 = TimeBasedWindowParameters(4,0,1)
val Sw = TimeBasedWindow apply (D,42,params0)
val Sw2 = TimeBasedWindow apply (D,42,4,0,1) //alternative input
val Sw3 = TimeBasedWindow apply (D,42,4) //=> D,42,4,0,1
//
val T0 = T
val a43 = Atom("exp(a3,m)")
val a44 = Atom("exp(a1,m)")
val v0 = Evaluation(v.mapping + (43 -> Set(a43),44 -> Set(a44)))
//
"S*"
val S = LStream(T0,v0)
val B = new HashSet[Atom]
val M = Structure(T0,v0,B)
//
"M,S*,43 ||- exp(a3,m) ?"
M/S/43 ||- a43
M/S/42 ||- diamond(a43)
M/S/42 ||- not(box(a43))
M/S/42 ||- (diamond(a43) or box(a43))
M/S/42 ||- (diamond(a43) and box(a43))
//
"M,S*,42 ||- @43 exp(a3,m) ?"
M/S/42 ||- At(43,a43)
//
"M,S*,42 ||- @43 exp(a3,m) ?"
M/S/42 ||- at(43) (a43)
val tau = TimeBasedWindow
val params1 = TimeBasedWindowParameters(0,5,1)
val w_5 = win(tau,ch2,params1)
//
"M,S*,42 ||- w+5 d exp(a3,m) ?"
M/S/42 ||- Window(tau,ch2,params1,diamond(a43)) //true
//
"M,S*,42 ||- w+5 d exp(a3,m) ?"
M/S/42 ||- w_5 (diamond(a43)) //true
//
"M,S*,10 ||- w+5 d exp(a3,m) ?"
M/S/10 ||- w_5 (diamond(a43)) //false
//
"M,S*,10 ||- (w+5 d exp(a3,m)) -> B(tram(a1,b)) and -(D(tram(a1,b)))?"
M/S/10 ||- ((w_5 (diamond(a43))) implies (box(a36) and not(diamond(a36)))) //true
M/10 |= ((w_5 (diamond(a43))) implies (box(a36) and not(diamond(a36)))) //true