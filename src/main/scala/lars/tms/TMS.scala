package lars.tms

import lars.core.ClosedIntInterval
import lars.core.semantics.formulas._
import lars.core.semantics.programs.extatoms._
import lars.core.semantics.programs.general.inspect.ExtensionalAtoms
import lars.core.semantics.programs.standard.{StdRule, StdProgram}
import lars.core.semantics.streams.{Evaluation, Timeline, S}
import lars.core.windowfn.WindowFunctionFixedParams
import lars.core.windowfn.time.{TimeWindowFixedParams, TimeWindowParameters, TimeWindow}
import lars.core.windowfn.tuple.{TupleWindowFixedParams, TupleWindow}
import lars.strat.Strata
import lars.tms.cons.{ConsW, ConsStar}
import lars.tms.incr.Result
import lars.tms.incr.Result.{fail, success}
import lars.tms.status.{Status, Label, Labels}
import lars.tms.status.Status.{in, unknown, out}
import scala.collection.mutable
import scala.collection.mutable.HashMap

/**
 * Created by hb on 6/25/15.
 */
case class TMS(P: StdProgram, N:Set[ExtendedAtom],J:Set[J]) {

  private val stratum: Map[Int, StdProgram] = Strata(P)
  private val n = stratum.keySet.reduce(math.max)
  var L = Labels()
  private var updated = Map[Int,Set[ExtendedAtom]]()
  private var waOperators:HashMap[Class[_ <:WindowFunctionFixedParams], WindowAtomOperators] = mutable.HashMap(classOf[TimeWindowFixedParams] -> new TimeWindowAtomOperators)

  init()

  def answerUpdate(t: Int, D: S, tp: Int, wAOp:HashMap[Class[_ <:WindowFunctionFixedParams], WindowAtomOperators] = mutable.HashMap()): Result = {
//    if(wAOp.nonEmpty) waOperators = wAOp
    println("initD: "+D)
    println("stratum: "+stratum)
    waOperators ++= wAOp
    val Lp = L.copy()
    for (l <- 1 to n) {
      var C = Set[WindowAtom]()
      println(l)
//      println("expired: "+Expired(D,l,tp,t))
      for ((alpha,omega) <- Expired(D,l,tp,t)) {
        println(alpha +" : "+omega)
        ExpireInput(alpha,omega,t)
        C = C + omega
      }
      println("fired: "+Fired(D,l,tp,t))
      for ((alpha,omega,t1) <- Fired(D,l,tp,t)) {
        FireInput(alpha,omega,t1,l,D)
        C = C + omega
      }
      UpdateTimestamps(C,Lp,l,t)
/*      SetUnknown(l,t)
      var madeNewAssignment = false
      do {
        if (SetRule(l,t) == fail) return fail
        val opt:Option[Boolean] = MakeAssignment(l,t)
        if (opt.isEmpty) {
          return fail
        }
        madeNewAssignment=opt.get
      } while (madeNewAssignment)
      SetOpenOrdAtomsOut(l,t)
      PushUp(l,t)*/
    }
    success
  }

  def init() = {
    initLabels()
    println("initL: "+L)

//    answerUpdate(0,S(Timeline(0,0)),0) //t'?
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


  /*
      * resultSet:Set[Atom,WindowAtom] = Set()
      * forall(omega) {
      * resultSet.addToSet(a=AtomFrom(omega),omega) where
      *                   a == exp(omega,t,Fired(l,t)) without Fired(l,t))
      * }*/
  def Expired(D:S, l:Int, tp:Int, t:Int): Set[(ExtendedAtom,WindowAtom)] = {
    var result = Set[(ExtendedAtom,WindowAtom)]()
    val omegaSet:Set[WindowAtom] = getOmega(P)

    if(omegaSet.isEmpty) return result
    for (omega <- omegaSet) {
      for (t1 <- tp to t) {
        val expSet = waOperators(omega.wop.wfn.getClass).exp(omega, L, t1, Fired(l, t1, omega.atom).get).getOrElse(Set())

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
      val Dp = S(tlp, D.v | tlp)

      for ((time, atom) <- Dp.getTimestampedAtoms()) {
        result = result ++ Fired(l, time, atom).getOrElse(Set())
      }
    }


    result
  }

  def Fired(l:Int, time:Int, atom:Atom): Option[Set[(ExtendedAtom,WindowAtom,Int)]] = {
    var result = Set[(ExtendedAtom,WindowAtom,Int)]()
    l match {
      case 0 => None
      case 1 =>
        for (ea <- ConsW(stratum(l), atom)) {
          val wat = wAtom(ea)
          if (wat.nonEmpty) result = result ++ Set((atom, wat.get, time))
        }
        val watStrat = getAt(stratum(l), atom, time)
        if (watStrat.nonEmpty) result = result ++ Set((AtAtom(watStrat.get.t, atom), watStrat.get, watStrat.get.t))
        Option(result)
      case _ => Option(Push(l,time))
    }
  }

  def wAtom(consW: ExtendedAtom): Option[WindowAtom] = consW match {
    case wa:WindowAtom => Option(wa)
    case _ => None
  }

  def getAt(stdP: StdProgram, a: Atom, t:Int): Option[WAt] = {

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
        for (atom <- updated(i)) {
          atom match {
            case ata: AtAtom =>
              var wa = wf(ata, l)
              if (wa.isEmpty) wa = wf(atom, l)
              if (wa.isDefined) {
                val intervals = L.intervals(ata)
                for (interval: ClosedIntInterval <- intervals) {
                  if (waOperators(wa.get.wop.wfn.getClass).aR(atom, wa.get, interval.lower, interval.upper).contains(ata.t)) {
                    result += ((ata, wa.get, ata.t))
                  }
                }
              }
            case a: Atom => val pn = PushNow(l, a); result += ((pn._1, pn._2, t))
          }
        }
      }
    }
    result
  }

  def PushNow(l: Int, atom: Atom): (ExtendedAtom,WindowAtom) = (atom,wf(atom, l).get)

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
    //TODO
  }

  def SetRule(l: Int, t: Int): Result = {
    //TODO
    fail
  }

  def MakeAssignment(l: Int, t: Int): Option[Boolean] = {
    //TODO
    None
  }

  def SetOpenOrdAtomsOut(l: Int, t: Int): Unit = {
    //TODO
  }

  def PushUp(l: Int, t: Int): Unit = {
    //TODO
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
