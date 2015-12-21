package lars.tms

import lars.core.semantics.formulas.{ExtendedAtom, Atom, Not}
import lars.core.semantics.programs.extatoms.{WindowAtom, WDiam, AtAtom, WAt}
import lars.core.semantics.programs.general.inspect.ExtensionalAtoms
import lars.core.semantics.programs.standard.{StdProgram, StdRule}
import lars.core.semantics.streams.{S, Evaluation, Timeline}
import lars.core.windowfn.time.TimeWindow
import lars.strat.Strata
import lars.tms.acons.ACons
import lars.tms.cons._
import lars.tms.status.rule.{fInval, fVal}
import lars.tms.status.{Labels, Label}
import lars.tms.status.Status.{out, in}
import lars.tms.supp.{SuppN, SuppP}
import org.scalatest.FunSuite

import scala.collection.immutable.HashMap

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
  val v = Evaluation(Map(m(37.2) -> Set(busG), m(39.1) -> Set(tramB), m(40.2) -> Set(expBusM),m(44.1) -> Set(expTrM)))
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


  test("Exp10") {

    val expTrmAt = AtAtom(m(44.1), expTrM)
    val trmBAt = AtAtom(m(39.1), tramB)
    val trmBW = WAt(wop5, m(39.1), tramB)
    val onAtom = on

    val r2p = r2g //StdRule(expTrmAt, Set(trmBW, onAtom))
    val Pp = StdProgram(Set(r2p))

    val consw = ConsW(Pp, trmBAt)
    val conshTrmB = ConsH(Pp, trmBW)
    val conshOn = ConsH(Pp, onAtom)
    val consAt = ConsAt(Pp, expTrmAt)

    assert(consw.size == 1 && consw.contains(WAt(wop5, m(39.1), tramB)))
    assert(conshTrmB.size == 1 && conshTrmB.contains(expTrmAt))
    assert(conshOn.size == 1 && conshOn.contains(expTrmAt))
    assert(consAt.size == 1 && consAt.contains(expTrM))

    val ltram = Label(in, (m(39.1), m(44.1)))
    val lOn = Label(in, (m(39.7), m(40.7)))
    val L = Labels(collection.mutable.Map(trmBW -> ltram, onAtom -> lOn))

    assert(fVal(L, r2p))

    L.update(expTrmAt, Label(in, (m(39.7), m(40.7))))

    val supp = SuppP(Pp, L, expTrmAt)
    val acons = ACons(Pp, L, trmBW)

    assert(supp == Set(trmBW, onAtom))
    assert(acons == Set(expTrmAt))

  }

  test("Exp15") {

    val expTrmAt = AtAtom(m(44.1), expTrM)
    val trmBAt = AtAtom(m(39.1), tramB)
    val trmBW:ExtendedAtom = WAt(wop5, m(39.1), tramB)
    val onAtom = on

    val v = Evaluation(Map(m(44.1) -> Set(expTrM), m(39.1) -> Set(tramB,on)))
    val D = S(T,v)

    val r2p = StdRule(expTrmAt, Set(trmBW, onAtom))
    val Pp1 = StdProgram(Set(r2p))
//    val Pp2 = StdProgram(StdRule(Set(WDiam(wopP5,expTrM))))

    val ltram = Label(in, (m(39.1), m(44.1)))
    val lOn = Label(in, (m(39.7), m(40.7)))
    val L = Labels(collection.mutable.Map(trmBW -> ltram, onAtom -> lOn))

    val t = m(39.7)

    val tms = TMS(Pp1)
    tms.init()
    tms.updateL(L)

/*    println(tms.stratum)
    tms.answerUpdate(t,D,m(37))
    println(tms.L)*/

/*    tms.updateL(L)

    val l = 1
    val tp = m(37)
    var C = Set[WindowAtom]()
    tms.setStratKey(l)

    for ((alpha,omega,t1) <- tms.Fired(D,l,tp,t)) {
      tms.FireInput(alpha,omega,t1,l,D)
      C = C + omega
      println("alph: "+alpha)
      tms.addToUpdated(alpha,l)
    }
    println("woop: "+tms.L)*/


//    val tms = TMS.apply(P)



//    tmsInit.L = L
//    val strat = Map[Int,StdProgram] = Map(1 -> Pp1, 2 -> Pp2)

      val answerupdate = tms.answerUpdate(m(0),m(45), D)
      println(answerupdate)


  }

  test("Exp11"){

//    val tms = TMS(P)
    var L = Labels()
    val stratum = Strata(P)

    val A: Set[Atom] = ExtensionalAtoms(P)
    for (a <- A) {
      L.update(a,Label(out, (0,0)))
    }

    val transCons: Set[ExtendedAtom] = A.flatMap(ConsStar(P,_))
    val outs = transCons.filter(!_.isInstanceOf[WindowAtom])
    for (x <- outs) {
      L.update(x,Label(out))
    }

//    println("tramB: "+ConsStar(P,tramB))
    assert(ConsStar(P,tramB) == Set(WAt(wop5,m(39.1),tramB),
                                    AtAtom(m(39.1)+m(5),expTrM),
                                    WDiam(wopP5,expTrM),
                                    expTrM,
                                    takeTrM,
                                    takeBusM))

//    println("busG: "+ConsStar(P,busG))
    assert(ConsStar(P,busG) == Set(WAt(wop3,m(37.2),busG),
                                    AtAtom(m(37.2)+m(3),expBusM),
                                    WDiam(wopP5,expBusM),
                                    expBusM,
                                    takeTrM,
                                    takeBusM))

//    println("jam: "+ConsStar(P,jam))
    assert(ConsStar(P,jam) == Set(WDiam(wop3,jam),
//                                  AtAtom(m(37.2)+m(3),expBusM),
//                                  WDiam(wopP5,expBusM),
//                                  expBusM,
                                  takeTrM,
                                  takeBusM))

//    println("request: "+ConsStar(P,request))
    assert(ConsStar(P,request) == Set(WDiam(wop1,request),
                                      AtAtom(m(39.1)+m(5),expTrM),
                                      AtAtom(m(37.2)+m(3),expBusM),
                                      WDiam(wopP5,expTrM),
                                      WDiam(wopP5,expBusM),
                                      on,
                                      expTrM,
                                      expBusM,
                                      takeTrM,
                                      takeBusM))


//    println("acons: "+ACons(P,L,A,0))
    assert(ACons(P,L,A,0) == A.flatMap(a => ConsStar(P,a)))


    println("stratum(0): "+stratum(0))
    println("stratum(1): "+stratum(1))

    val tms = TMS(P)

    val expired = tms.Expired(D,1,0,0)
    val fired = tms.Fired(D,1,0,0)

    val unkowns = tms.SetUnknown(1,0)
    val setrule = tms.SetRule(1,0)
    val sethead = tms.SetHead(on,on,1,0)

        println("labels: "+L)
    println(fInval(L,r3))
//    tms.answerUpdate(m(0),m(0),D)

  }

}
