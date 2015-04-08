package lars.exp

import lars.exp.Formulas._
import lars.exp.Streams._
import lars.exp.WindowFunctions.WindowFunction


/**
 * Created by hb on 1/2/15.
 */
//M
case class Structure(T: Timeline, v: Evaluation, B: Set[Atom]) {
  def /(stream: Stream) = StructureStream(this, stream)
}

//M,S
case class StructureStream(M: Structure, S: Stream) {
  def /(t: Int) = StructureStreamTimePoint(this, t)
}

//M,S,t
case class StructureStreamTimePoint(MS: StructureStream, t: Int) {
  def ||- (fm: Formula) : Boolean = {

    val M = MS.M
    val S0 = Stream(M.T,M.v)
    val S = MS.S
    val T = S.T
    val v:Map[Int,Set[Atom]] = M.v.map

    fm match {
      case a: Atom    => v.getOrElse(t,Set()).contains(a) || M.B.contains(a)
      case Not(a)     => !(this ||- a)
      case And(a, b)  => (this ||- a) && (this ||- b)
      case Or(a, b)   => (this ||- a) || (this ||- b)
      case Impl(a, b) => !(this ||- a) || (this ||- b)
      case D(a)       => T.timePoints exists { u => MS/u ||- a }
      case B(a)       => T.timePoints forall { u => MS/u ||- a }
      case At(u,a)    => (u in T) && (MS/u ||- a)
      case Win(w,ch,x,a) => {
        val S1 = w(ch(S0,S),t,x)
        M/S1/t ||- a
      }

    }
  }
}

