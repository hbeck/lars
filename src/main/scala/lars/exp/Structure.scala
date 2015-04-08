package lars.exp

import lars.exp.Formulas._
import lars.exp.Streams._


/**
 * Created by hb on 1/2/15.
 */
//M
case class Structure(T: Timeline, v: Evaluation, B: Set[Atom]) {
  def /(stream: Stream) = StructureStream(this, stream)
  def /(t: Int) = StructureTimePoint(this,t)
}

//M,S
case class StructureStream(M: Structure, S: Stream) {
  def /(t: Int) = StructureStreamTimePoint(M,S,t)
}

//M,t
case class StructureTimePoint(M: Structure, t: Int) {
  def |= (fm: Formula) : Boolean = {
    val S = Stream(M.T,M.v)
    val MSt = StructureStreamTimePoint(M,S,t)
    MSt ||- fm
  }
}

//M,S,t
case class StructureStreamTimePoint(M: Structure, S: Stream, t: Int) {

  def ||- (fm: Formula) : Boolean = {

    val S0 = Stream(M.T,M.v)
    val T = S.T
    val v:Map[Int,Set[Atom]] = M.v.map

    fm match {
      case a: Atom    => v.getOrElse(t,Set()).contains(a) || M.B.contains(a)
      case Not(a)     => !(this ||- a)
      case And(a, b)  => (this ||- a) && (this ||- b)
      case Or(a, b)   => (this ||- a) || (this ||- b)
      case Impl(a, b) => !(this ||- a) || (this ||- b)
      case D(a)       => T.timePoints exists { u => M/S/u ||- a }
      case B(a)       => T.timePoints forall { u => M/S/u ||- a }
      case At(u,a)    => (u in T) && (M/S/u ||- a)
      case Win(w,ch,x,a) => {
        val S1 = w(ch(S0,S),t,x)
        M/S1/t ||- a
      }

    }
  }
}

