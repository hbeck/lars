package lars.tms

import lars.core.ClosedIntInterval
import lars.core.semantics.formulas._
import lars.core.semantics.programs.extatoms._
import lars.core.semantics.programs.general.inspect.ExtensionalAtoms
import lars.core.semantics.programs.standard.inspect.PH
import lars.core.semantics.programs.standard.{StdProgram, StdRule}
import lars.core.semantics.streams.{S, Timeline}
import lars.core.windowfn.WindowFunctionFixedParams
import lars.core.windowfn.time.TimeWindowFixedParams
import lars.strat.Strata
import lars.tms.acons.ACons
import lars.tms.cons.{Cons, ConsStar, ConsW}
import lars.tms.incr.Result
import lars.tms.incr.Result.{fail, success}
import lars.tms.status.Status.{in, out, unknown}
import lars.tms.status.rule.{fInval, fVal, ufVal}
import lars.tms.status.{Label, Labels, Status}
import lars.tms.supp._

import scala.collection.immutable.HashMap

/**
 * Created by hb on 6/25/15.
 */
case class TMS(P: StdProgram) {

  private val stratum: Map[Int, StdProgram] = Strata(P)
  private val n = stratum.keySet.reduce(math.max)
  private var updated = Map[Int,Set[ExtendedAtom]]()
  private var waOperators:HashMap[Class[_ <:WindowFunctionFixedParams], WindowAtomOperators] =
    HashMap(classOf[TimeWindowFixedParams] -> TimeWindowAtomOperators)
  private var pushNow = Set[(WindowAtom,Int)]()
  private var push = Set[WindowAtom]()
  private var A = Set[Atom]()
  private var support = Support()

  def answerUpdate(L: Labels,tp: Int, t: Int, D: S,
                   wAOp:HashMap[Class[_ <:WindowFunctionFixedParams], WindowAtomOperators] = HashMap()): Option[Labels] = {

    waOperators ++= wAOp
    val Lp = L.copy

    for (l <- 1 to n) {
      var C = Set[WindowAtom]()
      for (omega <- Expired(D,l,tp,t,L)) {
        ExpireInput(omega,t,L)
        C += omega
        A += omega.atom
        addToUpdated(omega,l)
      }
      for ((omega,t1) <- Fired(D,l,tp,t,L)) {
        FireInput(omega,t1,l,D,L)
        C += omega
        A += omega.atom
        addToUpdated(omega,l)
      }
      UpdateTimestamps(C,L,Lp,l,t)
      val unknowns = SetUnknown(l,t,L,A)
      var madeNewAssignment = false
      do {
        if (SetRule(l,t,L,unknowns) == fail) return None
        val opt:Option[Boolean] = MakeAssignment(l,t,L,unknowns)
        if (opt.isEmpty) {
          return None
        }
        madeNewAssignment=opt.get
      } while (madeNewAssignment)
      SetOpenOrdAtomsOut(l,t,L)
      PushUp(l,t,L)
    }
    Option(L)
  }

  def addToUpdated(atom: ExtendedAtom, l: Int) = {
    val heads = stratum(l).rules exists {rule => rule.h.contains(atom)}
    if(heads) {
      updated += l -> (updated.getOrElse(l,Set()) ++ Set(atom))
    }
  }

  def init(): Labels = {
    val L = Labels()
    initLabels(L)
    initUpdated()
    answerUpdate(L,0,0,S(Timeline(0,0)))
    support = initSupport(L)
    L
  }

  def initLabels(L: Labels) = {
    val inputAtoms: Set[Atom] = ExtensionalAtoms(P)
    A = inputAtoms

    A foreach {a => L.update(a,Label(out, (0,0))) }
    val transCons: Set[ExtendedAtom] = A.flatMap(ConsStar(P,_))

    for(x <- transCons){
      x match {
        case wa:WindowAtom => L.update(x,Label(out,(0,0)))
        case _ => L.update(x,Label(unknown))
      }
    }
  }

  def initUpdated() = {
    for (i <- 1 to n) {
      updated += i -> Set()
    }
  }

  def initSupport(L: Labels): Support = {
    val support = Support()
    var programAtoms: Set[ExtendedAtom] = Set()
    for(rules <- P.rules) {
      programAtoms ++= (rules.B ++ Set(rules.h))
    }

    programAtoms foreach {
      a =>  support.suppP += a -> SuppP(P,L,a)
            support.suppN += a -> SuppN(P,L,a)
            support.suppAt += a -> SuppAt(P,L,a)
    }
    support
  }

  /*
      * resultSet:Set[Atom,WindowAtom] = Set()
      * forall(omega) {
      * resultSet.addToSet(a=AtomFrom(omega),omega) where
      *                   a == exp(omega,t,Fired(l,t)) \ Fired(l,t))
      * }*/
  def Expired(D: S, l:Int, tp:Int, t:Int, L:Labels): Set[WindowAtom] = {
    var result = Set[WindowAtom]()
    val omegaSet = getOmega(stratum(l))

    if (omegaSet.isEmpty) return result
    for (omega <- omegaSet) {
      for (t1 <- tp to t) {
        if(waOperators(omega.wop.wfn.getClass).exp(omega, L, t1, Fired(l,t1,D,L))) {
          result += omega
        }
      }
    } 
    result 
  }

  def getOmega(P: StdProgram): Set[WindowAtom] = {
    var result = Set[WindowAtom]()
    var pH = Set[ExtendedAtom]()

    P.rules.foreach(pH += _.h)
    (pH ++ P.rules.flatMap(_.B)) collect {case wa:WindowAtom => wa} foreach {result += _}
    result
  }

  def Fired(D:S, l:Int, tp:Int, t:Int, L: Labels): Set[(WindowAtom,Int)] = {
    var result = Set[(WindowAtom,Int)]()
    if(tp >= t) return Set()

    val tlp = Timeline(tp + 1, t)
    val Dp = S(tlp, D.v | tlp)

    for(t1 <- tp to t) {
      result ++= Fired(l,t1,Dp,L)
    }
    result
  }

  def Fired(l: Int, t1: Int, D: S, L: Labels): Set[(WindowAtom,Int)] = {
    l match {
      case 1 =>
        var result = Set[(WindowAtom,Int)]()
          for(a <- D(t1)){
            for(r <- stratum(l).rules){
              val tmp = (r.B ++ Set(r.h)) filter(p =>
                ConsW(stratum(l),a).contains(p) || p.contains(AtAtom(t1,a)))
              tmp.collect { case wa:WindowAtom => wa} foreach {wa => result += ((wa,t1))}
            }
          }
        result
      case _ =>
        Push(l,t1,L) ++ PushNow(l,t1)
    }
  }

  def Push(l: Int, t:Int, L:Labels): Set[(WindowAtom,Int)] = {
    var result = Set[(WindowAtom,Int)]()
    for (i <- 1 to l-1) {
      if (updated.contains(i)) {
        updated(i) collect {case a:AtAtom => a} foreach {
          ata =>
            for (iv <- tm(ata, L)) {
              (wf(ata, l) ++ wf(ata.atom, l)) foreach { a =>
                if (waOperators(a.wop.wfn.getClass).aR(a, iv, ata.t).contains(t)) {
                  result += ((a, ata.t))
                }
              }
            }
        }
      }
    }
    push foreach {r => result += ((r,t))}
    result
  }

  def PushNow(l: Int, t: Int): Set[(WindowAtom,Int)] = {
    var result: Set[(WindowAtom,Int)] = Set()
    for (i <- 1 to l-1) {
      if (updated.contains(i)) {
        updated(i) collect {case a:Atom => a} foreach {
          ea =>
            val wa = wf(ea, l).filterNot(a => a.nested.exists {case at:AtAtom => true} )
            wa foreach {a => result += ((a,t)) }
        }
      }
    }
    result ++ pushNow
  }

  /*modified wf function (see wf(a,w,l) in ijcai15-extended p.9)*/
 def wf(atom: ExtendedAtom, l: Int): Set[WindowAtom] = {
   ConsW(stratum(l),atom) collect {case wa:WindowAtom => wa}
 }

  def ExpireInput(omega: WindowAtom, t: Int, L: Labels): Unit = {
      if(!tm(omega,L).exists(e => e.upper > t)){
        val wfn = waOperators(omega.wop.wfn.getClass)
        val s_out = wfn.SOut(omega,t)
        L.update(omega, Label(out,(s_out.lower,s_out.upper)))
    }
  }

  def FireInput(omega: WindowAtom, t: Int, l:Int, D:S, L:Labels): Unit = {
    val ata = new AtAtom(t,omega.atom)
    //NOTE checks for @atoms within window atoms
    if (P.rules exists (r => r.B.exists(_.contains(ata)) || r.h.contains(ata))) {
      L.update(ata,Label(in,(t,t)))
    }
    //NOTE checks for @atoms directly in the program
/*      if (P.rules.exists(r => r.B.contains(ata) || r.h == ata)) {
          L.update(ata,Label(in,(t,t)))
        }*/

    val wfn = waOperators(omega.wop.wfn.getClass)
    val s_in:Option[ClosedIntInterval] = wfn.SIn(omega, t, l, D, L)

    if(s_in.isDefined) {
      L.update(omega, Label(in, (s_in.get.lower, s_in.get.upper)))
    }
  }

  def tm(a: ExtendedAtom, L:Labels): Set[ClosedIntInterval] = L.intervals(a)

  /*if ∀b ∈ B (r) : t ∈ tm(b)
      MinEnd (r, t) = min{t 2 | b ∈ B (r) ∧ tm(b) = [t 1 , t 2 ]};
    else
      MinEnd (r, t) = t.*/
  def MinEnd(r:StdRule, t:Int, L:Labels): Int = {
    var t2Set = Set[Int]()

    if (r.B forall (tm(_,L) exists (_.contains(t)))) {
        r.B foreach (tm(_,L) foreach (t2Set += _.upper))
      return t2Set.min
    }
    t
  }

  def UpdateTimestamps(C: Set[WindowAtom], L:Labels, Lp:Labels, l: Int, t: Int): Unit = {
    var ki, ko, i2o, o2i = Set[ExtendedAtom]()

    for (wa <- C) {
      val newStatus = L.status(wa)
      if (newStatus == Lp.status(wa)) {
        if (newStatus == in){
          ki += wa
        }
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

        if (u1 && u2) {
          val head = UpdateTimestamp(rule, in, t, L)
          if(head.isDefined) ki += head.get
        } else if (u1 && u3) {
          val head = UpdateTimestamp(rule,out,t,L)
          if(head.isDefined) ko += head.get
        }
      }
  }

  def UpdateTimestamp(r:StdRule, s:Status, t:Int, L:Labels): Option[ExtendedAtom] = {
    if (L.status(r.h) == s) {
      var newIntervals = Set[ClosedIntInterval]()

      for (interval <- L.intervals(r.h)) {
        if (interval.contains(t)) {
          newIntervals ++= Set(new ClosedIntInterval(interval.lower,MinEnd(r,t,L)))
        }
        newIntervals += interval
      }
      L.update(r.h,Label(s,newIntervals))
      return Option(r.h)
    }
    None
  }

  def SetUnknown(l: Int, t: Int, L:Labels, A: Set[Atom]): Set[ExtendedAtom] = {
    var unknowns:Set[ExtendedAtom] = Set()
    for (a <- ACons(stratum(l),L,A,support,t)) {
      if (!(L.intervals(a) exists (_.contains(t)))) {
        L.update(a, Label(unknown, L.intervals(a)))
        unknowns += a
      }
    }
    unknowns
  }

  def SetRule(l: Int, t: Int, L:Labels, unknowns: Set[ExtendedAtom]): Result = {
    for(a <- unknowns){
      if (SetHead(a,a,l,t,L) == fail) return fail
    }
    success
  }

  def SetHead(prev: ExtendedAtom, alpha: ExtendedAtom, l: Int, t: Int, L:Labels): Result = {
    val ph = PH(stratum(l),alpha)
    val timeSet = minTime(ph,t,L)

    if (ph.exists(r => fVal(L,r))) {
      if(timeSet.nonEmpty) {
        val tStar = timeSet.max
        L.update(alpha,Label(in,(t,tStar)))
        UpdateOrdAtom(alpha,in,L)
        alpha match {
          case wa:WindowAtom => /*do nothing*/
          case _ =>
              val suppAt = support.suppAt(alpha)
            suppAt.foreach {
              case ata:AtAtom => L.addInterval(ata,new ClosedIntInterval(ata.t,ata.t))
            }
        }
        addToUpdated(alpha,l)
        support.updateP(stratum(l),L)
      }
    } else if (ph.nonEmpty && ph.forall(r => fInval(L,r))) {
      if(timeSet.nonEmpty) {
        val tStar = timeSet.min
        L.update(alpha,Label(out,(t,tStar)))
        UpdateOrdAtom(alpha,out,L)
        support.updateN(stratum(l),L)
      }
    }
    Cons(stratum(l),alpha) foreach { beta =>
      if (prev != beta && beta != alpha && L.status(beta) == unknown) {
        if (SetHead(alpha, beta,l,t,L) == fail) {
          return fail
        }
      }
    }
    success
  }

  def minTime(rules: Set[StdRule], t: Int, L:Labels): Set[Int] = {
    var minSet = Set[Int]()
    rules.foreach(r => minSet += MinEnd(r,t,L))
    minSet
  }

  def UpdateOrdAtom(alpha: ExtendedAtom, s: Status, L:Labels): Unit = alpha match {
    case ata:AtAtom =>
      val a = ata.atom
      if (s == in) {
        if (L.status(a) == in) {
          L.update(a, Label(in, tm(a,L) + new ClosedIntInterval(ata.t, ata.t)))
        } else {
          L.update(a,Label(in,(ata.t,ata.t)))
        }
      } else if (s == out) {
        if (L.status(a) == in) {
          L.update(a,Label(out,tm(a,L) - new ClosedIntInterval(ata.t,ata.t)))
        } else {
          L.update(a,Label(unknown,(0,0)))
        }
      }
    case _ => None
  }

  def MakeAssignment(l: Int, t: Int, L:Labels, unknowns: Set[ExtendedAtom]): Option[Boolean] = {
    val stillUnknown = unknowns.filter(p => L.status(p) == unknown)

    for(alpha <- stillUnknown) {

        val ph = PH(stratum(l),alpha)
        if (ph.exists(r => ufVal(L,r))) {
          L.update(alpha,Label(in,(t,t)))
          for (beta <- ph.find(r => ufVal(L,r)).get.Bn) {
            if (L.status(beta) == unknown) L.update(beta,Label(out,(t,t)))
          }
          UpdateOrdAtom(alpha,in,L)
          addToUpdated(alpha,l)
          support.updateP(stratum(l),L)
          return Option(false)

        } else {
          L.update(alpha,Label(out,(t,t)))
          ph foreach {r =>
            r.Bp foreach {b =>
              if (L.status(b) == unknown) L.update(b,Label(out,(t,t)))
            }
          }
          UpdateOrdAtom(alpha,out,L)
          support.updateN(stratum(l),L)
          return Option(false)
        }
      }
    Option(true)
  }

  def SetOpenOrdAtomsOut(l: Int, t: Int, L:Labels): Unit = {
    stratum(l).rules foreach {
      rule => (rule.B ++ Set(rule.h)) foreach {
        a => if (L.status(a.atom) == unknown) {
          L.update(a.atom, Label(out, (t, t)))
        }
      }
    }
  }

  def PushUp(l: Int, t: Int, L: Labels): Unit = {
    pushNow = Set()
    push = Set()
    for (i <- l+1 to n) {
      pushNow ++= PushNow(i,t)
      val p = Push(i,t,L)
      p.foreach(r => push += r._1)
    }
  }

  /*--- Methods only used for testing purposes ---*/
    def getUpdated = updated
    def getSupport = support
                  /*--- end ---*/
}
