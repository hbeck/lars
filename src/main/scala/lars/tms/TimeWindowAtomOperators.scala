package lars.tms

import lars.core.ClosedIntInterval
import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.extatoms.{WBox, WDiam, WindowAtom}
import lars.core.windowfn.WindowFunctionFixedParams
import lars.core.windowfn.time.TimeWindowFixedParams
import lars.tms.status.Labels
import lars.tms.status.Status.in

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

  def q(omega: WindowAtom, L:Labels): Map[ExtendedAtom,Set[Int]] = {
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

  override def aR(atom: ExtendedAtom, wa: WindowAtom, lower: Int, upper: Int): ClosedIntInterval = wa.w.wfn match {
    case wfn: TimeWindowFixedParams =>

      wfn.x.u match {
        case 0 => new ClosedIntInterval(lower, upper)
        case n: Int => new ClosedIntInterval(lower - n, upper - n)
      }
  }
}
