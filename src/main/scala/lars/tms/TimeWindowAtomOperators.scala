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

  override def exp(omega: WindowAtom, L:Labels, t: Int, fired: Set[(WindowAtom, Int)]): Boolean = omega.wop.wfn match {
    case wfn:TimeWindowFixedParams =>

    val atp = q(omega, L)

    omega match {
      case wb:WBox => mapInAtoms(omega,fired,t,atp)
      case _ =>
        val N = wfn.x.u-wfn.x.l
        mapInAtoms(omega,fired,t+N,atp)
    }
  }

  def mapInAtoms(omega: WindowAtom, fired: Set[(WindowAtom, Int)], t: Int, atp: Set[Int]): Boolean = {
      if (!fired.contains((omega, t))) {
        if(atp.exists(t1 => t1 < t)) true //Option(omega.atom)
      }
    false
  }

  override def q(omega: WindowAtom, L:Labels): Set[Int] = {
    var res = Set[Int]()

    if(L.status(omega) == in){
      for(i <- L.intervals(omega)) {
        res = res ++ i.toSeq.toSet
      }
    }
    res
  }

  override def aR(wa: WindowAtom, interval: ClosedIntInterval, tp: Int): ClosedIntInterval = wa.wop.wfn match {
    case wfn: TimeWindowFixedParams =>

      wfn.x.u match {
        case 0 => new ClosedIntInterval(interval.lower, interval.upper)
        case n: Int => new ClosedIntInterval(tp - n, tp)
      }
  }

   override def SIn(wa: WindowAtom, tStar: Int, l: Int, D: S, L: Labels): Option[ClosedIntInterval] = wa.wop.wfn match {
     case wfn: TimeWindowFixedParams =>

       var result:Option[ClosedIntInterval] = None

       val Nl = wfn.x.l
       val Nu = wfn.x.u

       var t = tStar

       var at:Option[AtAtom] = None

       wa.nested.find({
         case a:AtAtom => {
           t = a.t
           at = Option(a)
         }; true
         case _ => false
       })

       wa match {
         case wb:WBox =>
           l match {
             case 1 =>
               for (t1 <- math.max(0, t - Nl) to t+Nu) {
                 if (!D.v(t1).contains(wa.atom)) return None
               }
             result = Option(new ClosedIntInterval(t,t))
             case _ =>
               if (L.intervals(wa).contains(new ClosedIntInterval(math.max(0,t-Nl),t+Nu)))
               result = Option(new ClosedIntInterval(t,t))
           }
         case _ => result = Option(new ClosedIntInterval(t-Nu, t+Nl))
       }

       if(at.isDefined) {

        val tmp = L.intervals(at.get).filter(e => e.lower <= result.get.upper || e.upper >= result.get.lower)
        /*if i am wrong, tmp may contain more than one element and this doesn't work right*/
        if (tmp.nonEmpty) {
          val min = math.max(tmp.head.lower, result.get.lower)
          val max = math.min(tmp.head.upper, result.get.upper)

          result = Option(new ClosedIntInterval(min, max))
        }
       }

      result
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
