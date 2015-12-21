package lars.tms

import lars.core.ClosedIntInterval
import lars.core.semantics.formulas._
import lars.core.semantics.programs.extatoms._
import lars.core.semantics.programs.general.inspect.ExtensionalAtoms
import lars.core.semantics.programs.standard.inspect.PH
import lars.core.semantics.programs.standard.{StdRule, StdProgram}
import lars.core.semantics.streams.{Timeline, S}
import lars.core.windowfn.WindowFunctionFixedParams
import lars.core.windowfn.time.TimeWindowFixedParams
import lars.strat.Strata
import lars.tms.acons.ACons
import lars.tms.cons.{Cons, ConsW, ConsStar}
import lars.tms.incr.Result
import lars.tms.incr.Result.{fail, success}
import lars.tms.status.rule.{ufVal, fInval, fVal}
import lars.tms.status.{Status, Label, Labels}
import lars.tms.status.Status.{in, unknown, out}
import lars.tms.supp.SuppAt
import scala.collection.immutable

/**
 * Created by hb on 6/25/15.
 */
case class TMS(P: StdProgram) {


  private var stratum: Map[Int, StdProgram] = Strata(P)
  private val n = stratum.keySet.reduce(math.max)
  private var L = Labels()
  private var updated = Map[Int,Set[ExtendedAtom]]()
  private var waOperators:immutable.HashMap[Class[_ <:WindowFunctionFixedParams], WindowAtomOperators] = immutable.HashMap(classOf[TimeWindowFixedParams] -> TimeWindowAtomOperators)
  private var pushNow = Set[WindowAtom]()
  private var push = Set[WindowAtom]()
  private var A = Set[Atom]()

  init()

  def answerUpdate(tp: Int, t: Int, D: S, wAOp:immutable.HashMap[Class[_ <:WindowFunctionFixedParams], WindowAtomOperators] = immutable.HashMap()): Result = {
    waOperators ++= wAOp
    val Lp = L.copy
//    val Lq:Labels = copyL(L)

    //println("tp: "+tp+" t: "+t)
    //println("stratum(0): "+stratum(0))
    //println("stratum(1): "+stratum(1))
    //println("stratum(2): "+stratum(2))

    for (l <- 1 to n) {
        //println("--- "+l+" ---")
        //println("updated: "+updated)
        //println("L: "+L)
      var C = Set[WindowAtom]()
      //println("expired: "+Expired(D,l,tp,t))
      for (omega <- Expired(D,l,tp,t)) {
        ExpireInput(omega,t)
        C = C + omega
        A += omega.atom
        addToUpdated(omega.nested,l)
      }
      //println("fired: "+Fired(D,l,tp,t))
      for ((omega,t1) <- Fired(D,l,tp,t)) {
        FireInput(omega,t1,l,D)
        //println("L: "+L)
        C = C + omega
        A += omega.atom
        //println("C: "+C)
        addToUpdated(omega.nested,l)
        //println("updated: "+updated)
      }
      UpdateTimestamps(C,Lp,l,t)
      SetUnknown(l,t)
      var madeNewAssignment = false
      do {
        if (SetRule(l,t) == fail) return fail
        //println("L after setRule: "+L)
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

  def addToUpdated(atoms: Set[ExtendedAtom], l: Int) = {
      //println("stratl: "+stratum(l))
    stratum(l).rules.foreach(a =>
      if(atoms.contains(a.h)) {
      updated += l -> (updated(l) ++ Set(a.h))
    })
  }

  def init() = {
    initLabels()
    initUpdated()
    //println("initL: "+L)
    answerUpdate(0,0,S(Timeline(0,0))) //t'?
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
      *                   a == exp(omega,t,Fired(l,t)) \ Fired(l,t))
      * }*/
  def Expired(D: S, l:Int, tp:Int, t:Int): Set[WindowAtom] = {
    var result = Set[WindowAtom]()
    val omegaSet:Set[WindowAtom] = getOmega(P)

    if (omegaSet.isEmpty) return result
    for (omega <- omegaSet) {
      for (t1 <- tp to t) {
        val exp = waOperators(omega.wop.wfn.getClass).exp(omega, L, t1, Fired(l,t1,D))

        if (exp.isDefined) result += omega
      }
    } 
    result 
  }

  def getOmega(P: StdProgram): Set[WindowAtom] = {
    var result = Set[WindowAtom]()
    var pHeads = Set[ExtendedAtom]()
    P.rules.foreach(pHeads += _.h)

    for (elem <- P.rules.flatMap(_.B) ++ pHeads) {
      elem match {
        case wa:WindowAtom => result += wa
        case _ => result
      }
    }
    result
  }

  //Don't use window functions See ijcai15-extended p.9 left column "Collecting Input"
  def Fired(D:S, l:Int, tp:Int, t:Int): Set[(WindowAtom,Int)] = {
    var result = Set[(WindowAtom,Int)]()
    if(tp+1 > t) return Set()

    val tlp = Timeline(tp + 1, t)
    val Dp = S(tlp, D.v | tlp)

    for(t1 <- tp to t) {
      result ++= Fired(l,t1,Dp)
    }

    result
  }

  def Fired(l: Int, t1: Int, D: S): Set[(WindowAtom,Int)] = {
    var result = Set[(WindowAtom,Int)]()
    l match {
      case 0 => return result
      case 1 =>
        val dAtoms = D(t1)
        if(dAtoms.nonEmpty) {
          dAtoms.foreach(a => {
            for (rule <- stratum(1).rules) {
              var atomSet = rule.B.filter(p => p.atom == a)
              if(rule.h.atom == a) atomSet += rule.h
              atomSet.foreach({
                case wat:WAt =>
                  if(wat.a == a) result += ((wat,t1))
                case _ => result
              })
            }
            ConsW(stratum(1),a).foreach({ case wa:WindowAtom => result += ((wa,t1))})
          })
        }
      case _ =>
        var pNow = Set[(WindowAtom,Int)]()
        for (elem <- PushNow(l)) {
          pNow += ((elem,t1))
        }
        result ++= Push(l,t1) ++ pNow
    }
    result
  }

/*  def wAtom(consW: ExtendedAtom): Option[WindowAtom] = consW match {
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
  }*/

  def Push(l: Int, t:Int): Set[(WindowAtom,Int)] = {
    var result = Set[(WindowAtom,Int)]()
    for (i <- 1 to l-1) {
      if (updated.contains(i)) {
        updated(i).foreach({
          case ata:AtAtom =>
            var wa = wf(ata, l)
            if (wa.isEmpty) wa = wf(ata.atom, l)
            if (wa.isDefined) {
              val intervals = L.intervals(ata)
              for (interval <- intervals) {
                if (waOperators(wa.get.wop.wfn.getClass).aR(ata.atom, wa.get, interval.lower, interval.upper).contains(ata.t)) {
                  result += ((wa.get, ata.t))
                }
              }
            }
        })
      }
    }
    push.foreach(r => result += ((r,t)))
    result
  }

  def PushNow(l: Int): Set[WindowAtom] = {
    /*(atom,wf(atom, l).get)*/
    var result = Set[WindowAtom]()
    for (i <- 1 to l-1) {
      if (updated.contains(i)) {
        updated(i).foreach(ea =>
          if (wf(ea.atom,l).isDefined) result += wf(ea.atom,l).get
        )
      }
    }
    result ++ pushNow
  }

  /*modified wf function (see wf(a,w,l) in ijcai15-extended p.9)*/
  def wf(atom: ExtendedAtom, l: Int): Option[WindowAtom] = {
      //println("consw: "+ConsW(stratum(l),atom))
    ConsW(stratum(l),atom).foreach({
      case wa:WindowAtom => return Option(wa)
    })
    None
  }

  def ExpireInput(omega: WindowAtom, t: Int): Unit = {
    var s_out = new ClosedIntInterval(0,0)

    val tInOmega = tm(omega).filter(e => e.upper > t)
    if (tInOmega.isEmpty) s_out = waOperators(omega.wop.wfn.getClass).SOut(omega,t)
    L.update(omega.atom, Label(out,(s_out.lower,s_out.upper)))
  }

  def FireInput(omega: WindowAtom, t: Int, l:Int, D:S): Unit = {
    val ata = new AtAtom(t,omega.atom)

    if (P.rules.exists(r => r.B.exists(ea => ea.nested.contains(ata)) || r.h.nested.contains(ata))) {
      L.update(ata,Label(in,(t,t)))
    }

    var s_in = waOperators(omega.wop.wfn.getClass).SIn(omega,t,l,D,tm(omega))
    if(s_in.isDefined) {
      omega.nested.foreach({
        case ata:AtAtom =>
          val tmp = L.intervals(ata).filter(e => e.lower <= s_in.get.upper || e.upper >= s_in.get.lower)
          /*if i am wrong, tmp may contain more than one element and this doesn't work right*/
          if (tmp.nonEmpty) {
            s_in = Option(new ClosedIntInterval(math.min(tmp.head.lower,s_in.get.lower),math.max(tmp.head.upper,s_in.get.upper)))
          }
        case _ => s_in
      })
      L.update(omega, Label(in, (s_in.get.lower, s_in.get.upper)))
    }
  }

  def tm(b: ExtendedAtom): Set[ClosedIntInterval] = b match {
    case wa:WindowAtom =>
      var r = Set[ClosedIntInterval]()
      wa.nested.foreach({
        case w:WindowAtom => r
        case a => r ++= L.intervals(a)})
        r
    case _ => L.intervals(b)
  }

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
      var newIntervals = Set[ClosedIntInterval]()

      for (interval <- L.intervals(r.h)) {
        if (interval.contains(t)) newIntervals ++= Set(new ClosedIntInterval(interval.lower,MinEnd(r,t)))
        newIntervals += interval
      }
      L.update(r.h,new Label(s,newIntervals))
    }
  }

  def SetUnknown(l: Int, t: Int): Unit = {
    val k = ACons(stratum(l),L,A,t)
    k.foreach(f =>
      if (!L.intervals(f).exists(_.contains(t)))
        L.update(f,Label(unknown,L.intervals(f)))
    )
  }

  def SetRule(l: Int, t: Int): Result = {
    ACons(stratum(l),L,A,t).foreach(f =>
      if (L.status(f) == unknown) SetHead(f,l,t)
    )
    success
  }

  def SetHead(alpha: ExtendedAtom, l: Int, t: Int): Unit = {
    if (PH(stratum(l),alpha).exists(r => fVal(L,r))) {

      val tStar = fRuleInterval(PH(stratum(l),alpha),t).max
      L.update(alpha,Label(in,(t,tStar)))
      UpdateOrdAtom(alpha,in)
      alpha match {
        case wa:WindowAtom =>
        case _ =>
          val suppAt = SuppAt(stratum(l),L,alpha)
          suppAt.foreach({case ata:AtAtom =>
              L.addInterval(ata,new ClosedIntInterval(ata.t,ata.t))
          })
      }
      updated += l -> (updated(l) ++ Set(alpha))
      /*Update Supp+ (alpha) as defined ???*/
    } else if (PH(stratum(l),alpha).forall(r => fInval(L,r))) {

      val tStar = fRuleInterval(PH(stratum(l),alpha),t).min
      L.update(alpha,Label(out,(t,tStar)))
      UpdateOrdAtom(alpha,out)
      /*Update Supp-(alpha) as defined ???*/
    }

    Cons(stratum(l),alpha).foreach(beta =>
      if (L.status(beta) == unknown) SetHead(beta,l,t))
  }

  def fRuleInterval(rules: Set[StdRule], t: Int): Set[Int] = {
    var rr=Set[StdRule]()

    for (rule <- rules) {
      if (fVal(L,rule)) rr += rule
    }
    var minSet = Set[Int]()
    rr.foreach(r => minSet += MinEnd(r,t))
    minSet
  }

  def UpdateOrdAtom(alpha: ExtendedAtom, s: Status): Unit = alpha match {
    case ata:AtAtom =>
      if (s == in) {
        if (L.status(ata.atom) == in) {
          L.update(ata, Label(in, tm(ata.atom) + new ClosedIntInterval(ata.t, ata.t)))
        } else {
          L.update(ata,Label(in,(ata.t,ata.t)))
        }
      } else if (s == out) {
        if (L.status(ata.atom) == in) {
          L.update(ata,Label(out,tm(ata.atom) - new ClosedIntInterval(ata.t,ata.t)))
        } else {
          L.update(ata.atom,Label(unknown,(0,0)))
        }
      }
    case _ => None
  }

  def MakeAssignment(l: Int, t: Int): Option[Boolean] = {
    val acons = ACons(stratum(l),L,A,t)
    if (acons.isEmpty) return Option(false)
    acons.foreach(alpha =>
      if (L.status(alpha) == unknown) {

        val ph = PH(stratum(l),alpha)
        if (ph.exists(r => ufVal(L,r))) {
          L.update(alpha,Label(in,(t,t)))
          for (beta <- ph.find(r => ufVal(L,r)).get.Bn) {
            if (L.status(beta) == unknown) L.update(beta,Label(out,(t,t)))
          }
          UpdateOrdAtom(alpha,in)
          updated += l -> (updated(l) ++ Set(alpha))
          /*update supp+(alpha) as defined*/
          return Option(false)
        } else {
          L.update(alpha,Label(out,(t,t)))
          ph.foreach(r =>
            r.Bp.foreach(b =>
              if (L.status(b) == unknown) L.update(b,Label(out,(t,t)))
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
    pushNow = Set()
    push = Set()
    for (i <- l+1 to n) {
      pushNow ++= PushNow(i)
      Push(i,t).foreach(r => push += r._1)
    }
  }

  /*--- Methods only used for testing purposes ---*/
  def updateL(labels: Labels) = {
    L = labels
  }

  def setStratKey(i: Int) = {
    stratum += (1 -> stratum(0))
  }
  /*--- end ---*/
}

object TMS {
/*  def apply(P: StdProgram): TMS = {
    TMS(P)
  }*/
}
