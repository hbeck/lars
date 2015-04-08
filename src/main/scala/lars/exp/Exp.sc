import lars.exp.Formulas._
import lars.exp.Streams._
import lars.exp.WindowFunctions._
import lars.exp._
import scala.collection.immutable.{HashSet, HashMap}
val T = Timeline(0,50)
val a36 = Atom("tram(a1,b)")
val a40 = Atom("tram(a3,h)")
val v = Evaluation(HashMap(36 -> Set(a36),40 -> Set(a40)))
val D = Stream(T,v) //Ex 2
"w(D,42,(4,0,1))" // Ex 3
val Sw = TimeBasedWindow apply (D,42,Seq(4,0,1))
//
val T0 = T
val a43 = Atom("exp(a3,m)")
val a44 = Atom("exp(a1,m)")
val v0 = Evaluation(v.map + (43 -> Set(a43),44 -> Set(a44)))
"S*"
val S = Stream(T0,v0)
val B = new HashSet[Atom]
val M = Structure(T0,v0,B)
"M,S*,43 ||- exp(a3,m) ?"
M/S/43 ||- a43
M/S/42 ||- d(a43);
M/S/42 ||- not(b(a43));
M/S/42 ||- (d(a43) or b(a43));
M/S/42 ||- (d(a43) and b(a43));
"M,S*,42 ||- @43 exp(a3,m) ?"
M/S/42 ||- At(43,a43)
"M,S*,42 ||- @43 exp(a3,m) ?"
M/S/42 ||- at(43) (a43)
val tau = TimeBasedWindow
val w_5 = win(tau,ch2,Seq(0,5,1))
"M,S*,42 ||- w+5 d exp(a3,m) ?"
M/S/42 ||- Win(tau,ch2,Seq(0,5,1),d(a43)) //true
"M,S*,42 ||- w+5 d exp(a3,m) ?"
M/S/42 ||- w_5 (d(a43)) //true
"M,S*,10 ||- w+5 d exp(a3,m) ?"
M/S/10 ||- w_5 (d(a43)) //false
"M,S*,10 ||- (w+5 d exp(a3,m)) -> B(tram(a1,b)) and -(D(tram(a1,b)))?"
M/S/10 ||- ((w_5 (d(a43))) implies (b(a36) and not(d(a36)))) //true
M/10 |= ((w_5 (d(a43))) implies (b(a36) and not(d(a36)))) //true

