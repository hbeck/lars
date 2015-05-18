package lars.core

import lars.core.Formulas._
import lars.core.LStreams._


/**
 * Created by hb on 1/2/15.
 */
//M
case class Structure(T: Timeline, v: Evaluation, B: Set[Atom]) {
  def /(stream: LStream) = StructureStream(this, stream)
  def /(t: Int) = StructureTimePoint(this,t)
}

//M,S
case class StructureStream(M: Structure, S: LStream) {
  def /(t: Int) = StructureStreamTimePoint(M,S,t)
}

//M,t
case class StructureTimePoint(M: Structure, t: Int) {
  def |= (fm: Formula) : Boolean = {
    val S = LStream(M.T,M.v)
    val MSt = StructureStreamTimePoint(M,S,t)
    MSt ||- fm
  }
}

//M,S,t
case class StructureStreamTimePoint(M: Structure, S: LStream, t: Int) {

  //entailment
  def ||- (fm: Formula) : Boolean = {

    val S0 = LStream(M.T,M.v)
    val T = S.T
    val v:Map[Int,Set[Atom]] = M.v.mapping

    fm match {
      case a: Atom        => v.getOrElse(t,Set()).contains(a) || M.B.contains(a)
      case Not(fm1)       => !(this ||- fm1)
      case And(fm1, fm2)  => (this ||- fm1) && (this ||- fm2)
      case Or(fm1, fm2)   => (this ||- fm1) || (this ||- fm2)
      case Impl(fm1, fm2) => !(this ||- fm1) || (this ||- fm2)
      case Diamond(fm1)   => T.timePoints exists { M/S/_ ||- fm1 }
      case Box(fm1)       => T.timePoints forall { M/S/_ ||- fm1 }
      case At(u,fm1)      => (u in T) && (M/S/u ||- fm1)
      case Window(w,ch,x,fm1) => {
        val S1 = w(ch(S0,S),t,x)
        M/S1/t ||- fm1
      }

    }
  }
}

