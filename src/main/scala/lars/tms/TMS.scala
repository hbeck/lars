package lars.tms

import lars.core.ClosedIntInterval
import lars.core.semantics.formulas._
import lars.core.semantics.programs.extatoms._
import lars.core.semantics.programs.general.inspect.ExtensionalAtoms
import lars.core.semantics.programs.standard.inspect.PH
import lars.core.semantics.programs.standard.{StdRule, StdProgram}
import lars.core.semantics.streams.{Evaluation, Timeline, S}
import lars.core.windowfn.WindowFunctionFixedParams
import lars.core.windowfn.time.{TimeWindowFixedParams, TimeWindowParameters, TimeWindow}
import lars.core.windowfn.tuple.{TupleWindowFixedParams, TupleWindow}
import lars.strat.Strata
import lars.tms.acons.ACons
import lars.tms.cons.{Cons, ConsW, ConsStar}
import lars.tms.incr.Result
import lars.tms.incr.Result.{fail, success}
import lars.tms.status.rule.{ufVal, fInval, fVal}
import lars.tms.status.{Status, Label, Labels}
import lars.tms.status.Status.{in, unknown, out}
import lars.tms.supp.SuppAt
import scala.collection.mutable
import scala.collection.mutable.HashMap

/**
 * Created by hb on 6/25/15.
 */
case class TMS(P: StdProgram, N:Set[ExtendedAtom],J:Set[J]) {

  private val stratum: Map[Int, StdProgram] = Strata(P)
  private val n = stratum.keySet.reduce(math.max)
  private val L = Labels()
  private var updated = Map[Int,Set[ExtendedAtom]]()
  private var waOperators:HashMap[Class[_ <:WindowFunctionFixedParams], WindowAtomOperators] = mutable.HashMap(classOf[TimeWindowFixedParams] -> new TimeWindowAtomOperators)

  init()

  def answerUpdate(t: Int, D: S, tp: Int, wAOp:HashMap[Class[_ <:WindowFunctionFixedParams], WindowAtomOperators] = mutable.HashMap()): Result = {
//    if(wAOp.nonEmpty) waOperators = wAOp
    println("initL: "+L)
    //println("initD: "+D)
    //println("stratum: "+stratum)
    waOperators ++= wAOp
    val Lp = L.copy()
    for (l <- 1 to n) {
      println("L: "+L)
      var C = Set[WindowAtom]()
//      //println(l)
//      //println("expired: "+Expired(D,l,tp,t))
      for ((alpha,omega) <- Expired(D,l,tp,t)) {
//        //println(alpha +" : "+omega)
        ExpireInput(alpha,omega,t)
        C = C + omega
        addToUpdated(alpha,l)
      }
//      //println("fired: "+Fired(D,l,tp,t))
      for ((alpha,omega,t1) <- Fired(D,l,tp,t)) {
        FireInput(alpha,omega,t1,l,D)
        C = C + omega
        addToUpdated(alpha,l)
      }
      UpdateTimestamps(C,Lp,l,t)
      SetUnknown(l,t)
      var madeNewAssignment = false
      do {
        if (SetRule(l,t) == fail) return fail
        val opt:Option[Boolean] = MakeAssignment(l,t)
//        println(opt)
        if (opt.isEmpty) {
          return fail
        }
        madeNewAssignment=opt.get
      } while (madeNewAssignment)
      SetOpenOrdAtomsOut(l,t)
      PushUp(l,t)
    }
    success
  }

  def addToUpdated(atom: ExtendedAtom, l: Int) = {
    if(stratum(l).rules.exists(_.h == atom)) {
      updated += l -> (updated(l) ++ Set(atom))
    }
  }

  def init() = {
    initLabels()
    initUpdated()
    answerUpdate(0,S(Timeline(0,0)),0) //t'?
  }

  def initLabels() = {
    val inputAtoms: Set[Atom] = ExtensionalAtoms(P)
    for (a <- inputAtoms) {
      L.update(a,Label(out, (0,0)))
    }

    val transCons: Set[ExtendedAtom] = inputAtoms.flatMap(ConsStar(P,_))
    val unknowns = transCons.filter(!_.isInstanceOf[WindowAtom])
    for (x <- unknowns) {
      L.update(x,Label(unknown))
    }
  }

  def initUpdated() = {
    for (i <- 1 to n) {
      updated += i -> Set()
    }
  }


  /*
      * resultSet:Set[Atom,WindowAtom] = Set()
      * forall(omega) {
      * resultSet.addToSet(a=AtomFrom(omega),omega) where
      *                   a == exp(omega,t,Fired(l,t)) without Fired(l,t))
      * }*/
  def Expired(D:S, l:Int, tp:Int, t:Int): Set[(ExtendedAtom,WindowAtom)] = {
    var result = Set[(ExtendedAtom,WindowAtom)]()
    val omegaSet:Set[WindowAtom] = getOmega(P)
    //println("omegaSet: "+omegaSet)

    if(omegaSet.isEmpty) return result
    for (omega <- omegaSet) {
      for (t1 <- tp to t) {
        val expSet = waOperators(omega.wop.wfn.getClass).exp(omega, L, t1, Fired(l, t1, omega.atom).get).getOrElse(Set())
//        //println("expSet: "+expSet)

        if (expSet.nonEmpty) {
          val expTuple = (expSet.head, omega)
          result += expTuple
        }
      }
    }
   result 
  }

  def getOmega(P: StdProgram): Set[WindowAtom] = {
    var result = Set[WindowAtom]()

    for (elem <- P.rules.flatMap(_.B)) {
      elem match {
        case wa:WindowAtom => result += wa
        case _ => result
      }
    }
    result
  }

  //Don't use window functions See ijcai15-extended p.9 left column "Collecting Input"
  def Fired(D:S, l:Int, tp:Int, t:Int): Set[(ExtendedAtom,WindowAtom,Int)] = {
    var result = Set[(ExtendedAtom,WindowAtom,Int)]()

    if(tp+1 <= t) {
      val tlp = Timeline(tp + 1, t)
//      //println("tlp: "+ tlp)
      val Dp = S(tlp, D.v | tlp)
/*      //println("Dp: "+ Dp)
      //println("stratum("+l+"): "+stratum(l))*/

      for ((time, atom) <- Dp.getTimestampedAtoms()) {
        result = result ++ Fired(l, time, atom).getOrElse(Set())
      }
    }
    result
  }

  def Fired(l:Int, time:Int, atom:ExtendedAtom): Option[Set[(ExtendedAtom,WindowAtom,Int)]] = {
    var result = Set[(ExtendedAtom,WindowAtom,Int)]()
    l match {
      case 0 => None
      case 1 =>
        for (ea <- ConsW(stratum(l), atom)) {
          val wat = wAtom(ea)
          if (wat.nonEmpty) result = result ++ Set((atom, wat.get, time))
        }
        val watStrat = getAt(stratum(l), atom, time)
        if (watStrat.nonEmpty) result = result ++ Set((AtAtom(watStrat.get.t, atom.atom), watStrat.get, watStrat.get.t))
        Option(result)
      case _ =>
        var pNow = Set[(ExtendedAtom,WindowAtom,Int)]()
        for(elem <- PushNow(l)){
          pNow += ((elem._1,elem._2,time))
        }
        Option(Push(l,time) ++ pNow)
    }
  }

  def wAtom(consW: ExtendedAtom): Option[WindowAtom] = consW match {
    case wa:WindowAtom => Option(wa)
    case _ => None
  }

  def getAt(stdP: StdProgram, a: ExtendedAtom, t:Int): Option[WAt] = {

    for (rule <- stdP.rules) {
      val headAndBody = Set(rule.h) ++ rule.B
      headAndBody match {
        case wa:WAt => if (a == wa.a && t == wa.t) return Option(wa)
        case _ => None
      }
    }
    None
  }


  def Push(l: Int, t:Int): Set[(ExtendedAtom,WindowAtom,Int)] = {
    var result = Set[(ExtendedAtom,WindowAtom,Int)]()
    for (i <- 1 to l-1) {
      if (updated.contains(i)){
        updated(i).foreach({
          case ata:AtAtom =>
            var wa = wf(ata, l)
            if (wa.isEmpty) wa = wf(ata.atom, l)
            if (wa.isDefined) {
              val intervals = L.intervals(ata)
              for (interval: ClosedIntInterval <- intervals) {
                if (waOperators(wa.get.wop.wfn.getClass).aR(ata.atom, wa.get, interval.lower, interval.upper).contains(ata.t)) {
                  result += ((ata, wa.get, ata.t))
                }
              }
            }
        })
      }
    }
    result
  }

  def PushNow(l: Int): Set[(Atom,WindowAtom)] = {
    /*(atom,wf(atom, l).get)*/
    var result = Set[(ExtendedAtom,WindowAtom)]()
    for(i <- 1 to l-1) {
      if(updated.contains(i)){
        updated(i).foreach({
          case a:Atom =>
           if(wf(a,l).isDefined) result += ((a,wf(a,l).get))
        })
      }
    }
  }

  /*modified wf function (see wf(a,w,l) in ijcai15-extended p.9)*/
  def wf(atom: ExtendedAtom, l: Int): Option[WindowAtom] = {
    for (a <- ConsW(stratum(l),atom)) {
      a match {
        case wa:WindowAtom => return Option(wa)
      }
    }
    None
  }

  def ExpireInput(alpha: ExtendedAtom, omega: WindowAtom, t: Int): Unit = {
    var s_out = new ClosedIntInterval(0,0)

    val tInOmega = tm(omega).filter(e => e.upper > t)
    if(tInOmega.isEmpty) s_out = waOperators(omega.wop.wfn.getClass).SOut(omega,t)
    L.update(alpha, Label(out,(s_out.lower,s_out.upper)))
  }

  def FireInput(alpha: ExtendedAtom, omega: WindowAtom, t1: Int, l:Int, D:S): Unit = {
    var s_in:Option[ClosedIntInterval] = None

    alpha match {
      case ata:AtAtom =>
        if (P.contains(ata)) L.update(ata,Label(in,(t1,t1)))
        s_in = waOperators(omega.wop.wfn.getClass).SIn(omega,ata.t,l,D,tm(alpha).toSet)
        val tmp = L.intervals(ata).filter(e => e.lower <= s_in.get.upper || e.upper >= s_in.get.lower)
        /*tmp may contain more than one element, if i am wrong*/
       if(tmp.nonEmpty) s_in = Option(tmp.head)
      case _ =>
        s_in = waOperators(omega.wop.wfn.getClass).SIn(omega,t1,l,D,tm(alpha).toSet)
    }
    if(s_in.isDefined) L.update(omega,Label(in,(s_in.get.lower,s_in.get.upper)))
  }

  def tm(b: ExtendedAtom) = L.intervals(b)

  def MinEnd(r:StdRule, t:Int): Int = {
    var t2Set = Set[Int]()
    var notAll:Boolean = true
    for (b <- r.B) {
      notAll = true
      for (interval <- tm(b)) {
        if (interval.contains(t)) {
          notAll = false
          t2Set += interval.upper
        }
      }
      if (notAll) return t
    }
    t2Set.min
  }

  //Labels provided globally
  def UpdateTimestamps(C: Set[WindowAtom], Lp:Labels, l: Int, t: Int): Unit = {
    var ki, ko, i2o, o2i = Set[ExtendedAtom]()

    for (wa <- C) {
      val newStatus = L.status(wa)

      if (newStatus == Lp.status(wa)) {
       if (newStatus == in) ki += wa
        else ko += wa
      } else {
        if (newStatus == in) o2i += wa
        else i2o += wa
      }
    }

     for (rule <- stratum(l).rules) {
       val u1 = (rule.Bp intersect i2o).isEmpty && (rule.Bn intersect o2i).isEmpty
       val u2 = (rule.Bp intersect ki).nonEmpty || (rule.Bn intersect ko).nonEmpty
       val u3 = (rule.Bp intersect ko).nonEmpty || (rule.Bn intersect ki).nonEmpty

       if (u1 && u2) UpdateTimestamp(rule,in,t)
       else if (u1 && u3) UpdateTimestamp(rule,out,t)
     }
  }

  def UpdateTimestamp(r:StdRule, s:Status, t:Int) = {
    if (L.status(r.h) == s) {
      var newIntervals = collection.mutable.Set[ClosedIntInterval]()

      for (interval <- L.intervals(r.h)) {
        if(interval.contains(t)) newIntervals += new ClosedIntInterval(interval.lower,MinEnd(r,t))
        newIntervals += interval
      }
      L.update(r.h,new Label(s,newIntervals))
    }
  }

  def SetUnknown(l: Int, t: Int): Unit = {
    val k = ACons(stratum(l),L,getA(l),t)
    k.foreach(f =>
      if(!L.intervals(f).exists(_.contains(t)))
        L.update(f,Label(unknown,L.intervals(f)))
    )
  }

  def SetRule(l: Int, t: Int): Result = {
    ACons(stratum(l),L,getA(l),t).foreach(f => 
      if(L.status(f) == unknown) SetHead(f,l,t)
    )
    success
  }
  
  def getA(l: Int): Set[Atom] = {

    var A = Set[Atom]()
    for(rule <- stratum(l).rules) {
      if(updated(l).contains(rule.h)){
        A = A ++ rule.body.atoms() ++ rule.head.atoms()
      }
    }
    A
  }

  def SetHead(alpha: ExtendedAtom, l: Int, t: Int): Unit = {
    if(PH(stratum(l),alpha).exists(r => fVal(L,r))) {

      val tStar = fRuleInterval(PH(stratum(l),alpha),t).max
      L.update(alpha,Label(in,(t,tStar)))
      UpdateOrdAtom(alpha,in)
      alpha match {
        case wa:WindowAtom =>
        case _ =>
          val suppAt = SuppAt(stratum(l),L,alpha)
          suppAt.foreach({case ata:AtAtom =>
              L.intervals(ata) += new ClosedIntInterval(ata.t,ata.t)
          })
      }
      updated += l -> (updated(l) ++ Set(alpha))
      /*Update Supp+ (alpha) as defined ???*/
    } else if(PH(stratum(l),alpha).forall(r => fInval(L,r))) {

      val tStar = fRuleInterval(PH(stratum(l),alpha),t).min
      L.update(alpha,Label(out,(t,tStar)))
      UpdateOrdAtom(alpha,out)
      /*Update Supp-(alpha) as defined ???*/
    }

    Cons(stratum(l),alpha).foreach(beta =>
      if(L.status(beta) == unknown) SetHead(beta,l,t))
  }

  def fRuleInterval(rules: Set[StdRule], t: Int): Set[Int] = {
    var rr=Set[StdRule]()

    for(rule <- rules) {
      if(fVal(L,rule)) rr += rule
    }
    var minSet = Set[Int]()
    rr.foreach(r => minSet += MinEnd(r,t))
    minSet
  }

  def UpdateOrdAtom(alpha: ExtendedAtom, s: Status): Unit = alpha match {
    case ata:AtAtom =>
      if(s == in) {
        if (L.status(ata.atom) == in) {
          L.update(ata, Label(in, tm(ata.atom) + new ClosedIntInterval(ata.t, ata.t)))
        } else {
          L.update(ata,Label(in,(ata.t,ata.t)))
        }
      } else if(s == out) {
        if(L.status(ata.atom) == in) {
          L.update(ata,Label(out,tm(ata.atom) - new ClosedIntInterval(ata.t,ata.t)))
        } else {
          L.update(ata.atom,Label(unknown,(0,0)))
        }
      }
    case _ => None
  }

  def MakeAssignment(l: Int, t: Int): Option[Boolean] = {
    val acons = ACons(stratum(l),L,getA(l),t)
    if(acons.isEmpty) return Option(false)
    acons.foreach(alpha =>
      if(L.status(alpha) == unknown){

        val ph = PH(stratum(l),alpha)
        if(ph.exists(r => ufVal(L,r))) {
          L.update(alpha,Label(in,(t,t)))
          for(beta <- ph.find(r => ufVal(L,r)).get.Bn){
            if(L.status(beta) == unknown) L.update(beta,Label(out,(t,t)))
          }
          UpdateOrdAtom(alpha,in)
          updated += l -> (updated(l) ++ Set(alpha))
          /*update supp+(alpha) as defined*/
          return Option(false)
        } else {
          L.update(alpha,Label(out,(t,t)))
          ph.foreach(r =>
            r.Bp.foreach(b =>
              if(L.status(b) == unknown) L.update(b,Label(out,(t,t)))
            )
          )
          UpdateOrdAtom(alpha,out)
          /*update supp-(alpha) as defined*/
          return Option(false)
        }
      })
    Option(true)
  }

  def SetOpenOrdAtomsOut(l: Int, t: Int): Unit = {
    stratum(l).rules.foreach(rule =>
      (rule.B ++ Set(rule.h)).foreach({
        case wa:WindowAtom => return
        case a:ExtendedAtom =>
          if (L.status(a) == unknown) L.update(a, Label(out, (t, t)))
      })
    )
  }

  def PushUp(l: Int, t: Int): Unit = {
    for (i <- l to n) {
    }
  }

}

object TMS {
  def apply(P: StdProgram): TMS = {
    var j = Set[J]()
    var nSet = Set[ExtendedAtom]()
    for (rule <- P.rules) {
        j += new J(rule.Bp, rule.Bn, rule.h)
      nSet += rule.h
    }
    TMS(P,nSet,j)
  }
}
