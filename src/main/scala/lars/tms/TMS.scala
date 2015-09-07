package lars.tms

import lars.core.ClosedIntInterval
import lars.core.semantics.formulas._
import lars.core.semantics.programs.extatoms._
import lars.core.semantics.programs.general.inspect.ExtensionalAtoms
import lars.core.semantics.programs.standard.{StdRule, StdProgram}
import lars.core.semantics.streams.{Evaluation, Timeline, S}
import lars.core.windowfn.time.{TimeWindowFixedParams, TimeWindowParameters, TimeWindow}
import lars.core.windowfn.tuple.{TupleWindowFixedParams, TupleWindow}
import lars.strat.Strata
import lars.tms.cons.{ConsW, ConsStar}
import lars.tms.incr.Result
import lars.tms.incr.Result.{fail, success}
import lars.tms.status.{Status, Label, Labels}
import lars.tms.status.Status.{in, unknown, out}

/**
 * Created by hb on 6/25/15.
 */
case class TMS(P: StdProgram, N:Set[ExtendedAtom],J:Set[J]) {

  private val stratum: Map[Int, StdProgram] = Strata(P)
  private val n = stratum.keySet.reduce(math.max)
  private val L = Labels()
  private var updated = Map[Int,Set[ExtendedAtom]]()

  init()

  def answerUpdate(t: Int, D: S, tp: Int): Result = {
    val Lp = L.copy()
    for (l <- 1 to n) {
      var C = Set[WindowAtom]()
      for ((alpha,omega) <- Expired(D,l,tp,t)) {
        ExpireInput(alpha,omega,t)
        C = C + omega
      }
      for ((alpha,omega,t1) <- Fired(D,l,tp,t)) {
        FireInput(alpha,omega,t1)
        C = C + omega
      }
      UpdateTimestamps(C,Lp,l,t)
      SetUnknown(l,t)
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
      PushUp(l,t)
    }
    success
  }

  def init() = {
    initLabels()
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


  /*
      * resultSet:Set[Atom,WindowAtom] = Set()
      * forall(omega) {
      * resultSet.addToSet(a=AtomFrom(omega),omega) where
      *                   a == exp(omega,t,Fired(l,t)) without Fired(l,t))
      * }*/
  def Expired(D:S, l:Int, tp:Int, t:Int): Set[(ExtendedAtom,WindowAtom)] = {
    var result = Set[(ExtendedAtom,WindowAtom)]()
    val omegaSet:Set[WindowAtom] = getOmega(P)

    for (omega <- omegaSet) {
      val expSet = exp(omega,t,Fired(l,t,omega.atom).get).getOrElse(Set())
      if (expSet.nonEmpty) {
        val expTuple = (expSet.head, omega)
        result += expTuple
      }
    }
   result 
  }

  def getOmega(P: StdProgram): Set[WindowAtom] = {
    var result = Set[WindowAtom]()

    for (elem <- P.rules.flatMap(_.B)) {
      elem match {
        case wa:WindowAtom => result += wa
      }
    }
    result
  }

  def q(omega: WindowAtom): Map[ExtendedAtom,Set[Int]] = {
    var result = collection.mutable.HashMap[ExtendedAtom,Set[Int]]()
    val as = omega.fm.atoms()

    for (a <- as) {
      var tmp = Set[Int]()
      if (L.status(a) == in) {
        val intervals = L.intervals(a)
        for (interval <- intervals) {
          val tp = interval.upper
          tmp = tmp ++ Set(tp)
        }
      }
      result += (a -> tmp)
    }
    result.toMap
  }

  def mapInAtoms(omega: WindowAtom, fired: Set[(ExtendedAtom, WindowAtom, Int)], t: Int, atp: Map[ExtendedAtom, Set[Int]]): Option[Set[ExtendedAtom]] = {
    var result = Set[ExtendedAtom]()

    for (atom <- omega.fm.atoms()) {
      if (!fired.contains((atom, omega, t))) {
        for (time <- atp(atom)) {
          if (time < t) {
            result += atom
          }
        }
      }
    }
    Option(result)
  }

  def exp(omega: WindowAtom, t: Int, fired: Set[(ExtendedAtom, WindowAtom, Int)]): Option[Set[ExtendedAtom]] = {
    val result = Set[ExtendedAtom]()
    val atp = q(omega)

    omega match {
      case wd:WDiam => {
        wd.w.wfn match {
          case tw:TimeWindowFixedParams => {
            var N = 0
            val lower = tw.x.l
            val upper = tw.x.u
            if (lower == 0) {
              N = upper
            } else {
              N = lower * -1
            }

            return mapInAtoms(omega,fired,t+N,atp)
          }
          case tuw:TupleWindowFixedParams => //TODO
        }
        Option(result)
      }
      case wb:WBox =>
        wb.w.wfn match {
          case tw:TimeWindowFixedParams => {
            return mapInAtoms(omega,fired,t,atp)

            /*for (atom <- omega.fm.atoms()) {
              if (!fired.contains((atom, omega, t))) {
                for (time <- atp(atom)) {
                  if (time < t) {
                    result += atom
                  }
                }
              }
            }*/
          }
          case tuw:TupleWindowFixedParams => //TODO
        }
        Option(result)
    }
  }

  //Don't use window functions See ijcai15-extended p.9 left column "Collecting Input"
  def Fired(D:S, l:Int, tp:Int, t:Int): Set[(ExtendedAtom,WindowAtom,Int)] = {
    var result = Set[(ExtendedAtom,WindowAtom,Int)]()

    val tlp = Timeline(tp+1,t)
    val Dp = S(tlp,D.v|tlp)

    for ((time,atom) <- Dp.getTimestampedAtoms()) {
      result = result ++ Fired(l,time, atom).getOrElse(Set())
    }

    result
  }

  /*maybe rename this method to avoid confusion*/
  def Fired(l:Int, time:Int, atom:Atom): Option[Set[(ExtendedAtom,WindowAtom,Int)]] = {
    var result = Set[(ExtendedAtom,WindowAtom,Int)]()
    l match {
      case 0 => None
      case 1 => {
        for (ea <- ConsW(stratum(l), atom)) {
          val wat = wAtom(ea)
          if (wat.nonEmpty) result = result ++ Set((atom, wat.get, time))
        }
        val watStrat = getAt(stratum(l), atom)
        if (watStrat.nonEmpty) result = result ++ Set((AtAtom(watStrat.get.t, atom), watStrat.get, watStrat.get.t))
        Option(result)
      }
      case _ => {
        //        val pn = PushNow(l)
        Option(Push(l,time))
      }
    }
  }

  def wAtom(consW: ExtendedAtom): Option[WindowAtom] = consW match {
    case wa:WindowAtom => Option(wa)
    case _ => None
  }

  def getAt(stdP: StdProgram, a: Atom/*, t:Int*/): Option[WAt] = {

    for (rule <- stdP.rules) {
      val headAndBody = Set(rule.h) ++ rule.B
      headAndBody match {
        case wa:WAt => if (a == wa.a /*&& t == wa.t*/) return Option(wa)
      }
    }
    None
  }


  def Push(l: Int, t:Int): Set[(ExtendedAtom,WindowAtom,Int)] = {
    var result = Set[(ExtendedAtom,WindowAtom,Int)]()
    for (i <- 1 to l-1) {
      for (atom <- updated(i)) {
        atom match {
          case ata:AtAtom => {
            var wa = wf(ata,l)
            if (wa.isEmpty) wa = wf(atom,l)
            if (wa.isDefined) {
              val intervals = L.intervals(ata)
              for (interval:ClosedIntInterval <- intervals) {
//                if (interval.contains(ata.t))
                  if (aR(atom,wa.get,interval.lower,interval.upper).contains(ata.t)) result += ((ata,wa.get,ata.t))
              }
            }
          }
          case a:Atom => val pn = PushNow(l,a); result += ((pn._1,pn._2,t))
        }
      }
    }
    result
  }

  def PushNow(l: Int, atom: Atom): (ExtendedAtom,WindowAtom) = {
    /*    var result = Set[(ExtendedAtom,WindowAtom)]()
        for (i <- 1 to l-1) {
          for (atom <- updated(i)) {*/
    /*result += */(atom,wf(atom, l).get)
    /*      }
        }
        result*/
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

  /*TODO aR for tuple-based windows?*/
  def aR(atom: ExtendedAtom, wa: WindowAtom, lower: Int, upper: Int): ClosedIntInterval = wa.w.wfn match {
    case twp:TimeWindowFixedParams => twp.x.u match {
      case 0 => new ClosedIntInterval(lower,upper)
      case n:Int => new ClosedIntInterval(lower-n,upper-n)
    }
  }

  def ExpireInput(alpha: ExtendedAtom, omega: WindowAtom, t: Int): Unit = {
    //TODO
  }

  def FireInput(alpha: ExtendedAtom, omega: WindowAtom, t1: Int): Unit = {
    alpha match {
      case ata:AtAtom => if (P.contains(ata)) L.update(ata,Label(in,(t1,t1)))
    }
     omega match {
       case wb:WBox =>
         wb.w.wfn match {
           case tw:TimeWindowFixedParams => //TODO
           case tuw:TupleWindowFixedParams => //TODO
         }
       case wd:WDiam =>
         wd.w.wfn match {
           case tw:TimeWindowFixedParams => L.update(omega,Label(in,(t1,t1+tw.x.l)))
           case tuw:TupleWindowFixedParams => //TODO
         }

     }
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
    var ki, ko, i2o, o2i = Set[WindowAtom]()

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
       val u1 = u123(rule, i2o, o2i, false)
       val u2 = u123(rule, ki, ko, true)
       val u3 = u123(rule, ko, ki, true)
       
       if (u1 && u2) UpdateTimestamp(rule,in,t)
       else if (u1 && u3) UpdateTimestamp(rule,out,t)
     }
  }

/*  def U1(rule: StdRule, i2o: Set[WindowAtom], o2i: Set[WindowAtom]): Boolean = {
    val bP = rule.Bp
    val bN = rule.Bn
    
    for (wa <- i2o) {
      if (bP.contains(wa)) return false 
    }
    for (wa <- o2i) {
      if (bN.contains(wa)) return false
    }
    true
  }*/

  def u123(rule: StdRule, sSet1: Set[WindowAtom], sSet2: Set[WindowAtom], bool:Boolean): Boolean = {
    val bP = rule.Bp
    val bN = rule.Bn
    for (wa <- sSet1) {
      if (bP.contains(wa)) return bool
    }
    for (wa <- sSet2) {
      if (bN.contains(wa)) return bool
    }
    !bool
  }

  def UpdateTimestamp(r:StdRule, s:Status, t:Int) = {
    if (L.label(r.h) == s) {
      var newIntervals = collection.mutable.Set[ClosedIntInterval]()
      val tStar = MinEnd(r,t)
      /*really do that for all interval end-points?*/
      for (interval <- L.intervals(r.h)) {
        newIntervals += new ClosedIntInterval(interval.lower,tStar)
      }
      val newLabel = new Label(s,newIntervals)
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
