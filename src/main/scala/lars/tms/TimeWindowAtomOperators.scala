package lars.tms

import lars.core.ClosedIntInterval
import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.extatoms.{AtAtom, WBox, WDiam, WindowAtom}
import lars.core.semantics.streams.S
import lars.core.windowfn.WindowFunctionFixedParams
import lars.core.windowfn.time.TimeWindowFixedParams
import lars.tms.status.Labels
import lars.tms.status.Status.in

import scala.collection.parallel.mutable

/**
 * Created by et on 09.09.15.
 */
class TimeWindowAtomOperators extends WindowAtomOperators{

  override def exp(omega: WindowAtom, L:Labels, t: Int, fired: Set[(ExtendedAtom, WindowAtom, Int)]): Option[Set[ExtendedAtom]] = omega.w.wfn match {
    case wfn:TimeWindowFixedParams =>

    val atp = q(omega, L)

    omega match {
      case wd:WDiam => {
        var N = 0
        val lower = wfn.x.l
        val upper = wfn.x.u
        if (lower == 0) {
          N = upper
        } else {
          N = lower * -1
        }
        return mapInAtoms(omega,fired,t+N,atp)
      }
      case wb:WBox => return mapInAtoms(omega,fired,t,atp)
    }
    None
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

  def q(omega: WindowAtom, L:Labels): Set[Int] = {
//    var result = collection.mutable.HashMap[ExtendedAtom,Set[Int]]()
    var res = Set[Int]()
    val c = omega.atom

    if(L.status(c) == in){
      for(i <- L.intervals(c)) {
        res = res ++ i.toSeq.toSet
      }
    }
    res

/*    val as = omega.fm.atoms()

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
    result.toMap*/
  }

  override def aR(atom: ExtendedAtom, wa: WindowAtom, lower: Int, upper: Int): ClosedIntInterval = wa.w.wfn match {
    case wfn: TimeWindowFixedParams =>

      wfn.x.u match {
        case 0 => new ClosedIntInterval(lower, upper)
        case n: Int => new ClosedIntInterval(lower - n, upper - n)
      }
  }

   override def SIn(wa: WindowAtom, t: Int, l: Int, D: S, tm: Set[ClosedIntInterval]): Option[ClosedIntInterval] = wa.w.wfn match {
     case wfn: TimeWindowFixedParams =>

       val N = wfn.x.l

       wa match {
         case wd:WDiam => Option(new ClosedIntInterval(t, t+N))
         case wb:WBox =>
           l match {
           case 1 =>
           for (t1 <- math.max (0, t - N) to t) {
           if (! D.v (t1).contains (wa.atom) ) return None
           }
           return Option (new ClosedIntInterval (t, t) )
           case _ =>
           if (tm.contains (new ClosedIntInterval (math.max (0, t - N), t) ) )
           return Option (new ClosedIntInterval (t, t) )
           }
       }
       None
  }

  override def SOut(wfn: WindowFunctionFixedParams, t: Int): ClosedIntInterval = {

    new ClosedIntInterval(0,0)
  }
}
