package lars.exp

import lars.exp.Formulas._
import lars.exp.Streams._
import lars.exp.WindowFunctions._

import scala.collection.immutable.{HashMap, HashSet, Set}

/**
 * Created by hb on 1/2/15.
 */
object Experiments {

  def main(args: Array[String]) {

    val T = Timeline(0,50)
    val a36 = Atom("tram(a1,b)")
    val a40 = Atom("tram(a3,h)")
    val v36 = 36 -> Set(a36)
    val v40 = 40 -> Set(a40)
    val v = Evaluation(HashMap(v36,v40))
    val D = Stream(T,v)

    println("D : "+D) // Ex 2

    val Sw = TimeBasedWindow apply (D,42,Seq(4,0,1))

    println("w(D,42,(4,0,1)) = "+Sw) // Ex 3

    val T0 = T
    val a43 = Atom("exp(a3,m)")
    val a44 = Atom("exp(a1,m)")
    val v43 = 43 -> Set(a43)
    val v44 = 44 -> Set(a44)
    val v0 = Evaluation(v.map + (v43,v44))
    val S = Stream(T0,v0)

    println("S* : "+S)

    val B = new HashSet[Atom]
    val M = Structure(T0,v0,B)

    val r1 = M/S/43 ||- a43

    println("M,S*,43 ||- exp(a3,m) ?: "+r1)

    val r2 = M/S/42 ||- d(a43);
    val r3 = M/S/42 ||- not(b(a43));
    val r4 = M/S/42 ||- (d(a43) or b(a43));
    val r5 = M/S/42 ||- (d(a43) and b(a43));

    Seq(r2,r3,r4,r5) foreach println

    val r6 = M/S/42 ||- At(43,a43)
    val r7 = M/S/42 ||- at(43) (a43)

    println("M,S*,42 ||- @43 exp(a3,m) ?: "+r6)
    println("M,S*,42 ||- @43 exp(a3,m) ?: "+r7)

    val tau = TimeBasedWindow
    val w_5 = win(tau,ch2,Seq(0,5,1))

    val r8  = M/S/42 ||- Win(tau,ch2,Seq(0,5,1),d(a43)) //true
    val r9  = M/S/42 ||- w_5 (d(a43)) //true
    val r10 = M/S/10 ||- w_5 (d(a43)) //false
    val r11 = M/S/10 ||- ((w_5 (d(a43))) implies (b(a36) and not(d(a36)))) //true

    println()
    println("M,S*,42 ||- w+5 d exp(a3,m) ?: "+r8)
    println("M,S*,42 ||- w+5 d exp(a3,m) ?: "+r9)
    println("M,S*,10 ||- w+5 d exp(a3,m) ?: "+r10)
    println("M,S*,10 ||- ... ?: "+r11)

  }
}


