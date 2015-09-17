package lars.tms

import lars.core.semantics.formulas.{Atom, Not}
import lars.core.semantics.programs.extatoms.{WDiam, AtAtom, WAt}
import lars.core.semantics.programs.standard.{StdProgram, StdRule}
import lars.core.semantics.streams.{S, Evaluation, Timeline}
import lars.core.windowfn.time.TimeWindow
import org.scalatest.FunSuite

/**
 * Created by et on 24.08.15.
 */
class TestTMS  extends FunSuite {

  object busG extends Atom
  object tramB extends Atom
  object expBusM extends Atom
  object expTrM extends Atom
  object on extends Atom
  object request extends Atom
  object takeBusM extends Atom
  object takeTrM extends Atom
  object jam extends Atom

  def m(i:Double) = (i*10*60).toInt

  val T = Timeline(0,m(50))
  val v = Evaluation(Map(m(37.2) -> Set(busG), m(39.1) -> Set(tramB)))
  val D = S(T,v)
  //
  def w = TimeWindow
  val wop3 = w.toOp(m(3))
  val wop5 = w.toOp(m(5))
  val wop1 = w.toOp(m(1))
  val wopP5 = w.toOp(m(0),m(5),1)
  val M = D.toStructure()

  val r1g = StdRule(AtAtom(m(37.2)+m(3),expBusM), Set(WAt(wop3,m(37.2),busG),on))
  val r2g = StdRule(AtAtom(m(39.1)+m(5),expTrM), WAt(wop5,m(39.1),tramB), on) //convenience variant
  val r3 = StdRule(on, WDiam(wop1,request))
  val r4 = StdRule(takeBusM, WDiam(wopP5,expBusM), Not(takeTrM), Not(WDiam(wop3,jam)))
  val r5 = StdRule(takeTrM, WDiam(wopP5,expTrM), Not(takeBusM))


  val P = StdProgram(Set(r1g,r2g,r3,r4,r5))


  test("Exp15"){

    val r2p = StdRule(AtAtom(m(44.1),expTrM),Set(WAt(wop5,m(39.1),tramB),on))
    val Pp = StdProgram(Set(r2p))

    val t = m(39.7)

    val tms = TMS
    tms.apply(Pp).init()
  }
}
