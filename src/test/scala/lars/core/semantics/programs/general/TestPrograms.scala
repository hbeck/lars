package lars.core.semantics.programs.general

import lars.core.semantics.formulas.{Not, Atom}
import lars.core.semantics.streams.S
import lars.core.semantics.structure.IsAnswerStream
import org.scalatest.FunSuite

/**
 * Created by hb on 7/10/15.
 */
class TestPrograms extends FunSuite {

  object x extends Atom
  object y extends Atom

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

}
