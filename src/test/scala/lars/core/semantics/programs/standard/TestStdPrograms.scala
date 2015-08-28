package lars.core.semantics.programs.standard

import lars.core.semantics.formulas._
import lars.core.semantics.programs.AS
import lars.core.semantics.programs.extatoms.AtAtom
import lars.core.semantics.streams.S
import lars.core.semantics.structure.IsAnswerStream
import org.scalatest.FunSuite

/**
 * Created by hb on 7/10/15.
 */
class TestStdPrograms extends FunSuite {

  object x extends Atom
  object y extends Atom
  object a extends Atom
  object b extends Atom

  test("simple") {

    val r = StdRule(x,y)
    val P = StdProgram(Set(r))

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

    val r = StdRule(x,Not(y))
    val P = StdProgram(Set(r))

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

    val r1 = StdRule(x,Not(y))
    val r2 = StdRule(y,Not(x))
    val P = StdProgram(Set(r1,r2))

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

  test("a <- b. @1b <-") {

    val r1 = StdRule(a, b)
    val r2 = StdRule(AtAtom(1, b), Verum)
    val P = StdProgram(Set(r1, r2))

    val D = S((0, 1))

    for (t <- 0 to 1) {
      println("" + t)
      for (as <- AS(P, D, t)) {
        println(as)
      }
    }

    val I0 = D + (1 -> b)
    val I1 = D + (1 -> b) + (1 -> a)

    assert(IsAnswerStream(I0,P,D,0))
    assert(IsAnswerStream(I1,P,D,1))

    assert(AS(P,D,0).size == 1)
    assert(AS(P,D,1).size == 1)

  }

}
