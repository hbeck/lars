package lars.tms.incr

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.extatoms.WindowAtom
import lars.core.semantics.programs.general.inspect.ExtensionalAtoms
import lars.core.semantics.programs.standard.StdProgram
import lars.core.semantics.streams.{Timeline, S}
import lars.strat.Strata
import lars.tms.TMS
import lars.tms.cons.ConsStar
import lars.tms.incr.Result.{fail, success}
import lars.tms.status.Status.{unknown, out}
import lars.tms.status.{Label, Labels}

/**
 * Created by hb on 7/16/15.
 */
class AnswerUpdate(P: StdProgram) {

  //
  private val stratum: Map[Int, StdProgram] = Strata(P)
  private val n = stratum.keySet.reduce(math.max)
  private val L = Labels()

  init()
  private val tms:TMS = TMS(P,L)

  def apply(t: Int, D: S, tp: Int): Result = {
    for (l <- 1 to n) {
      var C = Set[WindowAtom]()
      for ((alpha,omega) <- Expired(l,tp,t)) {
        ExpireInput(alpha,omega,t)
        C = C + omega
      }
      for ((alpha,omega,t1) <- Fired(l,tp,t)) {
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
    this.apply(0,S(Timeline(0,0)),0) //t'?
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

  def Fired(l:Int, tp:Int, t:Int): Set[(ExtendedAtom,WindowAtom,Int)] = {
    //TODO
    // Missing stream?
    Set[(ExtendedAtom,WindowAtom,Int)]()
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
    return None
  }

  def SetOpenOrdAtomsOut(l: Int, t: Int): Unit = {
    //TODO
  }

  def PushUp(l: Int, t: Int): Unit = {
    //TODO
  }

}
