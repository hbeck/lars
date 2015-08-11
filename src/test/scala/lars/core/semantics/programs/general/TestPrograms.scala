package lars.core.semantics.programs.general

import lars.core.semantics.formulas._
import lars.core.semantics.programs.AS
import lars.core.semantics.streams.{S, Timeline}
import lars.core.semantics.structure.IsAnswerStream
import lars.core.windowfn.time.WTime
import org.scalatest.FunSuite

/**
 * Created by hb on 7/10/15.
 */
class TestPrograms extends FunSuite {

  object x extends Atom
  object y extends Atom
  object p extends Atom
  object q extends Atom

  test("simple") {

    val r = GeneralRule(x,y)
    val P = GeneralProgram(Set(r))

    val S0 = S((0,0))

    assert(IsAnswerStream(S0,P,S0,0))

    val S1 = S0 + (0 -> y)
    val I1 = S1 + (0 -> x)

    assert(!IsAnswerStream(S1,P,S1,0))
    assert(IsAnswerStream(I1,P,S1,0))
    assert(!IsAnswerStream(I1,P,S1,0,Set(x)))
    assert(IsAnswerStream(S1,P,S1,0,Set(x)))
    assert(IsAnswerStream(S0 + (0 -> x),P,S0,0,Set(y)))

  }

  test("simple negation") {

    val r = GeneralRule(x,Not(y))
    val P = GeneralProgram(Set(r))

    val S0 = S((0,0))
    val Sx = S0 + (0 -> x)
    val Sy = S0 + (0 -> y)
    val Sxy = S0 + (0 -> x) + (0 -> y)

    assert(!IsAnswerStream(S0,P,S0,0))
    assert(IsAnswerStream(Sx,P,S0,0))
    assert(IsAnswerStream(Sy,P,Sy,0))
    assert(!IsAnswerStream(Sxy,P,Sy,0))

  }

  test("multiple models") {

    val r1 = GeneralRule(x,Not(y))
    val r2 = GeneralRule(y,Not(x))
    val P = GeneralProgram(Set(r1,r2))

    val S0 = S((0,0))
    val Sx = S0 + (0 -> x)
    val Sy = S0 + (0 -> y)
    val Sxy = S0 + (0 -> x) + (0 -> y)

    assert(!IsAnswerStream(S0,P,S0,0))
    assert(IsAnswerStream(Sx,P,S0,0))
    assert(IsAnswerStream(Sy,P,S0,0))
    assert(!IsAnswerStream(Sxy,P,S0,0))

    assert(IsAnswerStream(Sx,P,Sx,0))
    assert(IsAnswerStream(Sy,P,Sy,0))
    assert(!IsAnswerStream(Sxy,P,Sx,0))
    assert(!IsAnswerStream(Sxy,P,Sy,0))

    val as: Set[S] = AS(P,S0,0)

    assert(as.size==2)
    assert(as contains Sx)
    assert(as contains Sy)
  }

  test("head quantification") {
    val r1 = GeneralRule(Diam(y),x)
    val P1 = GeneralProgram(Set(r1))

    val D = S((0,2)) + (0 -> x)

    val as1: Set[S] = AS(P1,D,0)

    assert(as1.size == 3)
    assert(as1 contains D + (0 -> y))
    assert(as1 contains D + (1 -> y))
    assert(as1 contains D + (2 -> y))

    //

    val r2 = GeneralRule(Box(y),x)
    val P2 = GeneralProgram(Set(r2))

    val as2: Set[S] = AS(P2,D,0)

    assert(as2.size == 1)
    assert(as2 contains (D + (0 -> y) + (1 -> y) + (2 -> y)))
  }

  test("multiple models windows") {

    val w1  = WTime(1)
    val wp1 = WTime(0,1)
    val w2  = WTime(2)
    val wp2 = WTime(0,2)

    val r1 = GeneralRule(wp1(Diam(x)),Not(w2(Diam(y))))
    val r2 = GeneralRule(wp2(Diam(y)),Not(w1(Diam(x))))
    val P = GeneralProgram(Set(r1,r2))

    val D = S((0,2))

    val as: Set[S] = AS(P,D,0)

//    for (a <- as) {
//      println("\n"+a)
//    }
//
//    println("\nanswer streams: "+as.size)

    assert(as.size == 4)

  }

  test("flp weirdness") {

    //val f = (p -> p) -> ((q -> Falsum) -> p)

    val b = p -> p
    val h = (q -> Falsum) -> p

    val D = S(Timeline(0,0))

    val P = GeneralProgram(Set(GeneralRule(h,b)))

    val as = AS(P,D,0)

    assert(as.size == 2)

//    println("#as: "+as.size)
//
//    for (a <- as) {
//      println("\n"+a)
//    }

    assert(IsAnswerStream(D, P, D, 0) == false)
    val Ip = S((0,0), (0,p))
    assert(IsAnswerStream(Ip, P, D, 0))
    val Iq = S((0,0), (0,q))
    assert(IsAnswerStream(Iq, P, D, 0))
    val Ipq = S((0,0), (0,p), (0,q))
    assert(IsAnswerStream(Ipq, P, D, 0) == false)

  }

}
