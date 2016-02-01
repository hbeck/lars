package lars.tms

import lars.core.ClosedIntInterval
import lars.core.semantics.formulas.{ExtendedAtom, Atom, Not}
import lars.core.semantics.programs.extatoms.{WindowAtom, WDiam, AtAtom, WAt}
import lars.core.semantics.programs.general.inspect.ExtensionalAtoms
import lars.core.semantics.programs.standard.{StdProgram, StdRule}
import lars.core.semantics.streams.{S, Evaluation, Timeline}
import lars.core.windowfn.time.TimeWindow
import lars.strat.Strata
import lars.tms.acons.ACons
import lars.tms.cons._
import lars.tms.status.rule.{ufVal, ufInval, fInval, fVal}
import lars.tms.status.{Labels, Label}
import lars.tms.status.Status.{unknown, out, in}
import lars.tms.supp.{SuppN, SuppP}
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

  val stratum = Strata(P)

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

  val tms = TMS(P)
  val L = Labels()

  var A: Set[Atom] = ExtensionalAtoms(P)
  for (a <- A) {
    L.update(a,Label(out, (0,0)))
  }

  val transCons: Set[ExtendedAtom] = A.flatMap(ConsStar(P,_))
  val outs = transCons.filter(!_.isInstanceOf[WindowAtom])
  for (x <- outs) {
    L.update(x,Label(out))
  }

  test("Exp16"){




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
/*                                  AtAtom(m(37.2)+m(3),expBusM),
                                  WDiam(wopP5,expBusM),
                                  expBusM,*/
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

    println("acons: "+ACons(P,L,A,0))
    assert(ACons(P,L,A,0) == A.flatMap(a => ConsStar(P,a)))

/*    println("this is acons: "+ACons(stratum(1),L,A,0))

    println("stratum(0): "+stratum(0))
    println("stratum(1): "+stratum(1))*/

    val expired = tms.Expired(D,1,0,0,L)
    assert(expired == Set())

    val fired = tms.Fired(D,1,0,0)
    assert(fired == Set())

    val unknowns = tms.SetUnknown(1,0,L)

    val setrule = tms.SetRule(1,0,L)

    assert(SuppN(stratum(1),L,on) == r3.B)
    assert(SuppN(stratum(1),L,AtAtom(m(37.2)+m(3),expBusM)) == Set(WAt(wop3,m(37.2),busG)))
    assert(SuppN(stratum(1),L,AtAtom(m(39.1)+m(5),expTrM)) == Set(WAt(wop5,m(39.1),tramB)))

    tms.MakeAssignment(1,0,L)

    println("stratum(l): "+stratum(1))
    tms.SetOpenOrdAtomsOut(1,0,L)
    println("L: "+L)

    /*revisit if this is actually the case*/
    assert(L.status(expTrM) == out)
    assert(L.status(expBusM) == out)

    /**Stratum 2**/
    assert(L.status(WDiam(wopP5,expTrM)) == out)
    assert(L.status(WDiam(wopP5,expBusM)) == out)

    assert(ufInval(L,r4))
    assert(ufInval(L,r5))
  }

  test("Exp17"){

/*    val tms = TMS(P)
    val L = Labels()*/

    val t = m(37.2)
    A = v(t) //{busG}

//    println("A: "+A)

    val fired = tms.Fired(D,1,t,m(37.2))
//    println("fired: "+fired)
    tms.FireInput(fired.head._1,fired.head._2,1,D,L)
    assert(L.status(WAt(wop3,t,busG)) == in)
    assert(L.intervals(WAt(wop3,t,busG)) == Set(new ClosedIntInterval(t,m(40.2))))
//    println("A tms: "+tms.A)
    tms.SetUnknown(1,t,L)

    println(ACons(P,L,A,t))
    println(ACons(stratum(1),L,A,t))
    println(ACons(stratum(2),L,A,t))

/*    println("cons bus"+Cons(P,AtAtom(m(37.2),busG)))
    println("supp bus"+SuppN(P,L,AtAtom(m(37.2),busG)))*/

    tms.SetRule(1,t,L)

    assert(L.status(on) == out)
    assert(fInval(L,r1g))

    assert(L.status(AtAtom(m(37.2)+m(3),expBusM)) == out)
    assert(L.intervals(AtAtom(m(37.2)+m(3),expBusM)) == Set(new ClosedIntInterval(t,t)))

    tms.SetOpenOrdAtomsOut(1,t,L)

    assert(L.status(expBusM) == unknown)

    /** Stratum 2 **/

    assert(L.status(WDiam(wopP5,expTrM)) == out)
    assert(L.status(WDiam(wopP5,expBusM)) == out)

    assert(ufInval(L,r4))
    assert(ufInval(L,r5))

  }

  test("Exp18"){

/*    val tms = TMS(P)
    val L = Labels()*/
    val t = m(39.1)

    val fired = tms.Fired(D,1,t,t)

    tms.FireInput(fired.head._1,fired.head._2,1,D,L)
    assert(L.status(WAt(wop5,m(39.1),tramB)) == in)
    assert(L.intervals(WAt(wop5,m(39.1),tramB)) == Set(new ClosedIntInterval(t,m(44.1))))

  }

  test("Exp19"){

/*    val tms = TMS(P)
    val L = Labels()*/
    val t = m(39.7)


    /*update the stream and add the request at t = 39.7m*/
    val v = this.v ++ Evaluation(Map(t -> Set(request)))
    val D = S(T,v)
    A = v(t) //{request}

    val fired = tms.Fired(D,1,t,t)
    tms.FireInput(fired.head._1,fired.head._2,1,D,L)
    assert(L.status(WDiam(wop1,request)) == in)
    assert(L.intervals(WDiam(wop1,request)) == Set(new ClosedIntInterval(t,m(40.7))))

    tms.SetUnknown(1,t,L)

    /*replace with assertion*/
    println(ACons(P,L,A,t))

    assert(fVal(L,r3))
    tms.SetRule(1,t,L)

    assert(L.status(on) == in)
    assert(L.intervals(on) == Set(new ClosedIntInterval(t,m(40.7))))
    assert(SuppP(P,L,on) == Set(WDiam(wop1,request)))

    for(a <- Cons(stratum(1),on)) {
      if(a != AtAtom(m(37.2)+m(3),expBusM) && a != AtAtom(m(39.1)+m(5),expTrM)) {
        assert(L.status(a) == out)
        assert(L.intervals(a) == Set(new ClosedIntInterval(t,t)))
      }
    }

    assert(fVal(L,r1g))
    assert(fVal(L,r2g))

    /*expBusM*/
    assert(L.status(r1g.h) == in)
    assert(L.intervals(r1g.h) == Set(new ClosedIntInterval(t,t)))

    /*expTrM*/
    assert(L.status(r1g.h) == in)
    assert(L.intervals(r1g.h) == Set(new ClosedIntInterval(t,m(40.7))))

    tms.SetOpenOrdAtomsOut(1,t,L)
    assert(L.status(expBusM) == in)
    assert(L.intervals(expBusM) == Set(new ClosedIntInterval(m(40.2),m(40.2))))

    assert(L.status(expTrM) == in)
    assert(L.intervals(expTrM) == Set(new ClosedIntInterval(m(44.1),m(44.1))))

    assert(tms.Push(2,t,L) == Set((AtAtom(m(37.2)+m(3),expBusM),m(35.2)),(WDiam(wopP5,expBusM),m(35.2)),
                                  (AtAtom(m(39.1)+m(5),expTrM),m(39.1)),(WDiam(wopP5,expTrM),m(39.1))))

    val fired2 = tms.Fired(D,2,t,t)
    assert(fired2 == Set((AtAtom(m(37.2)+m(3),expBusM),m(35.2)),(WDiam(wopP5,expBusM),m(35.2)),
                                    (AtAtom(m(39.1)+m(5),expTrM),m(39.1)),(WDiam(wopP5,expTrM),m(39.1))))

    fired2.foreach(f => tms.FireInput(f._1,f._2,2,D,L))
    assert(L.status(WDiam(wopP5,expBusM)) == in)
    assert(L.intervals(expBusM) == Set(new ClosedIntInterval(t,m(40.7))))
    assert(L.status(WDiam(wopP5,expTrM)) == in)
    assert(L.intervals(expTrM) == Set(new ClosedIntInterval(t,m(40.7))))

    tms.SetUnknown(2,t,L)
    assert(L.status(takeTrM) == unknown)
    assert(L.status(takeBusM) == unknown)

    tms.SetRule(2,t,L)

    tms.MakeAssignment(2,t,L)
    assert(ufVal(L,r4))
    assert(ufVal(L,r5))
  }

  test("Exp20"){

    val t = m(40)

    val v = this.v ++ Evaluation(Map(t -> Set(jam)))
    val D = S(T,v)
    A = v(t) //{jam}

    println(ACons(P,L,A,t))
  }



}
