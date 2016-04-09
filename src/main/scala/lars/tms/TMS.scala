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
import lars.tms.supp.{SuppN, SuppAt}
import scala.collection.immutable

/**
 * Created by hb on 6/25/15.
 */
case class TMS(P: StdProgram) {


  private val stratum: Map[Int, StdProgram] = Strata(P)
  private val n = stratum.keySet.reduce(math.max)
//  private var L = Labels()
  private var updated = Map[Int,Set[ExtendedAtom]]()
  private var waOperators:immutable.HashMap[Class[_ <:WindowFunctionFixedParams], WindowAtomOperators] = immutable.HashMap(classOf[TimeWindowFixedParams] -> TimeWindowAtomOperators)
  private var pushNow = Set[WindowAtom]()
  private var push = Set[WindowAtom]()
  private var A = Set[Atom]()

  init()

  def answerUpdate(L: Labels,tp: Int, t: Int, D: S, wAOp:immutable.HashMap[Class[_ <:WindowFunctionFixedParams], WindowAtomOperators] = immutable.HashMap()): Result = {
    waOperators ++= wAOp
    val Lp = L.copy

    for (l <- 1 to n) {
      var C = Set[WindowAtom]()
      for (omega <- Expired(D,l,tp,t,L)) {
        ExpireInput(omega,t,L)
        C = C + omega
        A += omega.atom
        addToUpdated(omega.nested,l)
      }
      for ((omega,t1) <- Fired(D,l,tp,t,L)) {
        FireInput(omega,t1,l,D,L)
        C = C + omega
        A += omega.atom
        addToUpdated(omega.nested,l)
      }
      UpdateTimestamps(C,L,Lp,l,t)
      SetUnknown(l,t,L,A)
      var madeNewAssignment = false
      do {
        if (SetRule(l,t,L,A) == fail) return fail
        val opt:Option[Boolean] = MakeAssignment(l,t,L)
        if (opt.isEmpty) {
          return fail
        }
        madeNewAssignment=opt.get
      } while (madeNewAssignment)
      SetOpenOrdAtomsOut(l,t,L)
      PushUp(l,t,L)
    }
    success
  }

  def addToUpdated(atoms: Set[ExtendedAtom], l: Int) = {
    stratum(l).rules.foreach(a =>
      if(atoms.contains(a.h)) {
      updated += l -> (updated(l) ++ Set(a.h))
    })
  }

  def init() = {
    val L = Labels()
    initLabels(L)
    initUpdated()
    //println("initL: "+L)
    answerUpdate(L,0,0,S(Timeline(0,0))) //t'?
  }

  def initLabels(L: Labels) = {
    val inputAtoms: Set[Atom] = ExtensionalAtoms(P)
    A = inputAtoms

    for (a <- inputAtoms) {
      L.update(a,Label(out, (0,0)))
    }

    val transCons: Set[ExtendedAtom] = inputAtoms.flatMap(ConsStar(P,_))

    for(x <- transCons){
      if(x.isInstanceOf[WindowAtom]) {
        L.update(x,Label(out))
      } else {
        L.update(x,Label(unknown))
      }
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
    (pH ++ P.rules.flatMap(_.B)).foreach({
      case wa:WindowAtom => result += wa
      case _ => result
    })
    result
  }

  //Don't use window functions See ijcai15-extended p.9 left column "Collecting Input"
  def Fired(D:S, l:Int, tp:Int, t:Int, L: Labels): Set[(WindowAtom,Int)] = {
    var result = Set[(WindowAtom,Int)]()
    if(tp >= t) return Fired(l,t,D,L) //is this ok, or should it return an empty set?

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
        val dAtoms = D(t1)
        if(dAtoms.nonEmpty) {
          dAtoms.foreach(a => {
            val consw = ConsW(stratum(l),a)
            stratum(l).rules.foreach(r => {
              val tmp = (r.B ++ Set(r.h)).filter(p => consw.contains(p) || p.nested.contains(AtAtom(t1,a)))
              tmp.foreach({
                case wa:WindowAtom => result += ((wa,t1))
              })
            })
          })
        }
        result
      case _ =>
        var pNow = Set[(WindowAtom,Int)]()
        for (elem <- PushNow(l)) {
          pNow += ((elem,t1))
        }
        Push(l,t1,L) ++ pNow
    }
  }

  def Push(l: Int, t:Int, L:Labels): Set[(WindowAtom,Int)] = {
    var result = Set[(WindowAtom,Int)]()
    for (i <- 1 until l-1) {
        updated(i).foreach({
          case ata:AtAtom =>
            val wa:WindowAtom = wf(ata, l).getOrElse(wf(ata.atom, l).orNull)
            if (wa != null) {
             tm(ata,L).foreach(iv =>
               if (waOperators(wa.wop.wfn.getClass).aR(ata.atom, wa, iv).contains(ata.t)) {
                  result += ((wa, ata.t))
               }
             )
            }
        })
    }
    push.foreach(r => result += ((r,t)))
    result
  }

  def PushNow(l: Int): Set[WindowAtom] = {
    /*(atom,wf(atom, l).get)*/
    var result = Set[WindowAtom]()
    for (i <- 1 until l-1) {
        updated(i).foreach(ea => result += wf(ea.atom,l).orNull)
    }
    result ++ pushNow
  }

  /*modified wf function (see wf(a,w,l) in ijcai15-extended p.9)*/
  def wf(atom: ExtendedAtom, l: Int): Option[WindowAtom] = {
    ConsW(stratum(l),atom).foreach({
      case wa:WindowAtom => return Option(wa)
    })
    None
  }

  def ExpireInput(omega: WindowAtom, t: Int, L: Labels): Unit = {
    var s_out = new ClosedIntInterval(0,0)

    val tInOmega = tm(omega,L).filter(e => e.upper > t)
    if (tInOmega.isEmpty) s_out = waOperators(omega.wop.wfn.getClass).SOut(omega,t)
    L.update(omega.atom, Label(out,(s_out.lower,s_out.upper)))
  }

  def FireInput(omega: WindowAtom, t: Int, l:Int, D:S, L:Labels): Unit = {
    val ata = new AtAtom(t,omega.atom)

    //NOTE checks for @atoms within window atoms
    if (P.rules.exists(r => r.B.exists(ea => ea.nested.contains(ata)) || r.h.nested.contains(ata))) {
      L.update(ata,Label(in,(t,t)))
    }

    //NOTE checks for @atoms directly in the program
/*        if (P.rules.exists(r => r.B.contains(ata) || r.h == ata)) {
          L.update(ata,Label(in,(t,t)))
        }*/

    var s_in = waOperators(omega.wop.wfn.getClass).SIn(omega,t,l,D,tm(omega,L))
    if(s_in.isDefined) {
      omega.nested.foreach({
        case ata:AtAtom =>
          val tmp = L.intervals(ata).filter(e => e.lower <= s_in.get.upper || e.upper >= s_in.get.lower)
          /*if i am wrong, tmp may contain more than one element and this doesn't work right*/
          if (tmp.nonEmpty) {
            val min = math.min(tmp.head.lower,s_in.get.lower)
            val max = math.max(tmp.head.upper,s_in.get.upper)

            s_in = Option(new ClosedIntInterval(min,max))
          }
        case _ => s_in
      })
      L.update(omega, Label(in, (s_in.get.lower, s_in.get.upper)))
    }
  }

  def tm(b: ExtendedAtom, L:Labels): Set[ClosedIntInterval] = L.intervals(b)

  /*if ∀b ∈ B (r) : t ∈ tm(b)
      MinEnd (r, t) = min{t 2 | b ∈ B (r) ∧ tm(b) = [t 1 , t 2 ]};
    else
      MinEnd (r, t) = t.*/
  def MinEnd(r:StdRule, t:Int, L:Labels): Int = {
    var t2Set = Set[Int]()

    if(r.B.forall(b => tm(b,L).exists(i => i.contains(t)))){
      r.B.foreach(b =>  tm(b,L).foreach(i => t2Set += i.upper))
      return t2Set.min
    }
    t
  }

  def UpdateTimestamps(C: Set[WindowAtom], L:Labels, Lp:Labels, l: Int, t: Int): Unit = {
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

       if (u1 && u2) UpdateTimestamp(rule,in,t,L)
       else if (u1 && u3) UpdateTimestamp(rule,out,t,L)
     }
  }

  def UpdateTimestamp(r:StdRule, s:Status, t:Int, L:Labels) = {
    if (L.status(r.h) == s) {
      var newIntervals = Set[ClosedIntInterval]()

      for (interval <- L.intervals(r.h)) {
        if (interval.contains(t)) newIntervals ++= Set(new ClosedIntInterval(interval.lower,MinEnd(r,t,L)))
        newIntervals += interval
      }
      L.update(r.h,new Label(s,newIntervals))
    }
  }

  def SetUnknown(l: Int, t: Int, L:Labels, A: Set[Atom]): Unit = {
    println("set unknown acons: "+ACons(stratum(l),L,A,t))
    val k = ACons(stratum(l),L,A,t)
    k.foreach(f =>
     if (!L.intervals(f).exists(_.contains(t)))
         L.update(f, Label(unknown, L.intervals(f)))
    )
  }

  def SetRule(l: Int, t: Int, L:Labels, A: Set[Atom]): Result = {
    ACons(stratum(l),L,A,t).foreach(f =>
      if (L.status(f) == unknown) {
        if(SetHead(f,f,l,t,L) == fail) return fail
      }
    )
    success
  }

  def SetHead(prev: ExtendedAtom, alpha: ExtendedAtom, l: Int, t: Int, L:Labels): Result = {
//    println("stratum("+l+"): "+stratum(l))
    val ph = PH(stratum(l),alpha)
    val timeSet = minTime(ph,t,L)

//    if(t>0) ph.foreach(r => println("fval check - "+ r +": "+fVal(L,r)))
    if (ph.exists(r => fVal(L,r))) {
      if(t>0) println("fVal - alpha: "+alpha)
      if(timeSet.nonEmpty) {
        val tStar = timeSet.max
        if(t>0) println("fVal - alpha: "+alpha+", time: "+t+", tStar: "+tStar)
//        tStar = minTime(ph,t,L).max
        L.update(alpha,Label(in,(t,tStar)))
        println("L.status("+alpha+"): "+L.label(alpha))
        UpdateOrdAtom(alpha,in,L)
        alpha match {
          case wa:WindowAtom => /*do nothing*/
          case _ =>
            val suppAt = SuppAt(stratum(l),L,alpha)
            suppAt.foreach({case ata:AtAtom =>
                            L.addInterval(ata,new ClosedIntInterval(ata.t,ata.t))
                          })
        }
        updated += l -> (updated(l) ++ Set(alpha))
      }
    } else if (ph.nonEmpty && ph.forall(r => fInval(L,r))) {

      if(timeSet.nonEmpty) {
        val tStar = timeSet.min
//        if(t>0) println("fInval - alpha: "+alpha+", time: "+t+", tStar: "+tStar)
        L.update(alpha,Label(out,(t,tStar)))
        UpdateOrdAtom(alpha,out,L)
      }
    } /*else {
      return fail
    }*/

/*    println("alpha: "+alpha)
    println("Cons("+alpha+"): "+Cons(stratum(l),alpha))*/
    Cons(stratum(l),alpha).foreach(beta =>
      if (prev != beta && beta != alpha && L.status(beta) == unknown) {
        if(SetHead(alpha, beta,l,t,L) == fail){
          return fail
        }
      })
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
          println("L updateOrdAtom("+ata+"): "+L.intervals(ata))
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

  def MakeAssignment(l: Int, t: Int, L:Labels): Option[Boolean] = {
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
          UpdateOrdAtom(alpha,in,L)
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
          UpdateOrdAtom(alpha,out,L)
          /*update supp-(alpha) as defined*/
          return Option(false)
        }
      })
    Option(true)
  }

  def SetOpenOrdAtomsOut(l: Int, t: Int, L:Labels): Unit = {
    stratum(l).rules.foreach(rule =>
      (rule.B ++ Set(rule.h)).foreach({
        case a:ExtendedAtom =>
          if (L.status(a.atom) == unknown) {
            L.update(a.atom, Label(out, (t, t)))
          }
        case _ =>
      })
    )
  }

  def PushUp(l: Int, t: Int, L: Labels): Unit = {
    pushNow = Set()
    push = Set()
    for (i <- l+1 to n) {
      pushNow ++= PushNow(i)
      Push(i,t,L).foreach(r => push += r._1)
    }
  }

  /*--- Methods only used for testing purposes ---*/
    def getUpdated = updated
  /*--- end ---*/
}

object TMS {
/*  def apply(P: StdProgram): TMS = {
    TMS(P)
  }*/
}
