package lars.tms

import lars.core.semantics.formulas.WindowOperators.{ch2, StreamChoice}
import lars.core.semantics.formulas._
import lars.core.semantics.programs.extatoms._
import lars.core.semantics.programs.general.inspect.ExtensionalAtoms
import lars.core.semantics.programs.standard.{StdRule, StdProgram}
import lars.core.semantics.streams.{Evaluation, Timeline, S}
import lars.core.windowfn.WindowFunctionFixedParams
import lars.core.windowfn.partition.{PartitionWindowParameters, PartitionWindow}
import lars.core.windowfn.time.{TimeWindowParameters, TimeWindow}
import lars.strat.Strata
import lars.tms.cons.{ConsW, ConsStar}
import lars.tms.incr.Result
import lars.tms.incr.Result.{fail, success}
import lars.tms.status.{Label, Labels}
import lars.tms.status.Status.{unknown, out}
import lars.tms.status.rule.fVal

/**
 * Created by hb on 6/25/15.
 */
case class TMS(P: StdProgram, N:Set[ExtendedAtom],J:Set[J]) {

  private val stratum: Map[Int, StdProgram] = Strata(P)
  private val n = stratum.keySet.reduce(math.max)
  private val L = Labels()
  private var previousStream:S = S(Timeline(0,0))

  init()

  def answerUpdate(t: Int, D: S, tp: Int): Result = {
    previousStream = D
    for (l <- 1 to n) {
      var C = Set[WindowAtom]()
      for ((alpha,omega) <- Expired(l,tp,t)) {
        ExpireInput(alpha,omega,t)
        C = C + omega
      }
      for ((alpha,omega,t1) <- Fired(D,l,tp,t)) {
        FireInput(alpha,omega,t1)
        C = C + omega
      }
      UpdateTimestamps(C,l,t)
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

  def Expired(l:Int, tp:Int, t:Int): Set[(ExtendedAtom,WindowAtom)] = {
    //TODO
    Set[(ExtendedAtom,WindowAtom)]()
  }

  def wAtom(consW: Set[ExtendedAtom], atom:Atom): Option[WindowAtom] = {
    for (elem <- consW) {
     elem match {
       case wat:WAt =>
       case wa:WindowAtom => return Option(wa)
     }
    }
    None
  }

  def getAt(stdP: StdProgram, a: Atom): Option[WAt] = {

   for(rule <- stdP.rules){
        rule.B match {
          case wa:WAt => if(a == wa.a) return Option(wa)
        }
      }
    None
  }

  def PushNow(l: Int): (ExtendedAtom,WindowAtom) = {

  }

  def Push(l: Int) = {

  }

  //Don't use window functions See ijcai15-extended p.9 left column "Collecting Input"
  def Fired(D:S, l:Int, tp:Int, t:Int): Set[(ExtendedAtom,WindowAtom,Int)] = {
    var result = Set[(ExtendedAtom,WindowAtom,Int)]()

    val tlp = Timeline(tp+1,t)
    val Dp = S(tlp,D.v|tlp)

    for ((time,atom) <- Dp.getTimestampedAtoms()) {
    l match {
      case 0 => Set()
      case 1 => {
          val wat = wAtom(ConsW(P, atom), atom)
          if (wat.nonEmpty) {
            result = result ++ Set((atom, wat.get, time))
          }
          val watStrat = getAt(stratum(l), atom)
          if (watStrat.nonEmpty) {
            result = result ++ Set((AtAtom(watStrat.get.t, atom), watStrat.get, watStrat.get.t))
          }
        }
      case _ => val pn = PushNow(l)
        result = result ++ Set((pn._1,pn._2,time))
        if(time <= t) Push(l)
    }
    }
/*    val Dp = TimeWindow(previousStream,tp+1,TimeWindowParameters(tp+1,t,1))

    val wofp = WindowOperatorFixedParams(TimeWindow.fix(TimeWindowParameters(tp+1,t,1)))
    val stdPL = stratum(l)

    for ((time,atom) <- Dp.getTimestampedAtoms()) {
      if (stdPL.contains(atom)) {
        result = result ++ Set((atom, WAt(wofp, time, atom), time))
      } else {
        previousStream = previousStream - (time,atom)
      }
    }*/
    result
  }

  def ExpireInput(alpha: ExtendedAtom, omega: WindowAtom, t: Int): Unit = {
    //TODO
  }

  def FireInput(alpha: ExtendedAtom, omega: WindowAtom, t1: Int): Unit = {
    //TODO
  }

  //Labels provided globally
  def UpdateTimestamps(C: Set[WindowAtom], l: Int, t: Int): Unit = {
    //TODO
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
