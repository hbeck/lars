package lars.tms

import lars.core.ClosedIntInterval
import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.extatoms.{AtAtom, WBox, WDiam, WindowAtom}
import lars.core.semantics.streams.S
import lars.core.windowfn.time.TimeWindowFixedParams
import lars.tms.status.Labels
import lars.tms.status.Status.in

import scala.collection.parallel.mutable

/**
 * Created by et on 09.09.15.
 */
class TimeWindowAtomOperators extends WindowAtomOperators{

  override def exp(omega: WindowAtom, L:Labels, t: Int, fired: Set[(ExtendedAtom, WindowAtom, Int)]): Option[Set[ExtendedAtom]] = omega.wop.wfn match {
    case wfn:TimeWindowFixedParams =>

      println(L)
    val atp = q(omega, L)
      println("atp: "+atp)

    omega match {
      case wd:WDiam =>
        var N = 0
        val lower = wfn.x.l
        val upper = wfn.x.u
        if (lower == 0) {
          N = upper
        } else {
          N = lower * -1
        }
        mapInAtoms(omega,fired,t+N,atp)
      case wb:WBox => mapInAtoms(omega,fired,t,atp)
      case _ => None
    }
  }

  def mapInAtoms(omega: WindowAtom, fired: Set[(ExtendedAtom, WindowAtom, Int)], t: Int, atp: Set[Int]): Option[Set[ExtendedAtom]] = {
    var result = Set[ExtendedAtom]()

    for (atom <- omega.fm.atoms()) {
      if (!fired.contains((atom, omega, t))) {
        for (time <- atp) {
          if (time < t) {
            result += atom
          }
        }
      }
    }
    Option(result)
  }

  override def q(omega: WindowAtom, L:Labels): Set[Int] = {
    var res = Set[Int]()
    val c = omega.atom

    if(L.status(c) == in){
      for(i <- L.intervals(c)) {
        res = res ++ i.toSeq.toSet
      }
    }
    res
  }

  override def aR(atom: ExtendedAtom, wa: WindowAtom, lower: Int, upper: Int): ClosedIntInterval = wa.wop.wfn match {
    case wfn: TimeWindowFixedParams =>

      wfn.x.u match {
        case 0 => new ClosedIntInterval(lower, upper)
        case n: Int => new ClosedIntInterval(lower - n, upper - n)
      }
  }

   override def SIn(wa: WindowAtom, t: Int, l: Int, D: S, tm: Set[ClosedIntInterval]): Option[ClosedIntInterval] = wa.wop.wfn match {
     case wfn: TimeWindowFixedParams =>

       val Nl = wfn.x.l
       val Nu = wfn.x.u

       wa match {
         case wd:WDiam => Option(new ClosedIntInterval(t-Nu, t+Nl))
         case wb:WBox =>
           l match {
             case 1 =>
               for (t1 <- math.max(0, t - Nl) to t+Nu) {
                 if (!D.v(t1).contains(wa.atom)) return None
               }
             return Option(new ClosedIntInterval(t,t))
             case _ =>
               if (tm.contains(new ClosedIntInterval(math.max(0,t-Nl),t+Nu)))
               return Option(new ClosedIntInterval(t,t))
           }
       }
       None
  }

  override def SOut(wa: WindowAtom, t: Int): ClosedIntInterval = wa.wop.wfn match {
    case wfn: TimeWindowFixedParams =>

    wa match {
      case wd:WDiam => new ClosedIntInterval(t,t)
      case wb:WBox =>
        val Nl = wfn.x.l
        val Nu = wfn.x.u

        new ClosedIntInterval(t-Nu,t+Nl)
    }
  }
}
