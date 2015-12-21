package lars.tms

import lars.core.ClosedIntInterval
import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.extatoms._
import lars.core.semantics.streams.S
import lars.core.windowfn.time.TimeWindowFixedParams
import lars.tms.status.Labels
import lars.tms.status.Status.in

import scala.collection.parallel.mutable

/**
 * Created by et on 09.09.15.
 */
object TimeWindowAtomOperators extends WindowAtomOperators{

  override def exp(omega: WindowAtom, L:Labels, t: Int, fired: Set[(WindowAtom, Int)]): Option[ExtendedAtom] = omega.wop.wfn match {
    case wfn:TimeWindowFixedParams =>

    val atp = q(omega, L)

    omega match {
      case wb:WBox => mapInAtoms(omega,fired,t,atp)
      case _ =>
        val N = wfn.x.u-wfn.x.l
        mapInAtoms(omega,fired,t+N,atp)
    }
  }

  def mapInAtoms(omega: WindowAtom, fired: Set[(WindowAtom, Int)], t: Int, atp: Set[Int]): Option[ExtendedAtom] = {
      if (!fired.contains((omega, t))) {
        atp.foreach(t1 => if(t1 < t) return Option(omega.atom))
      }
    None
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
         case _ => return Option(new ClosedIntInterval(t-Nu, t+Nl))
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
