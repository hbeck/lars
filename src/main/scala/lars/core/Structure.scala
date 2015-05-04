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
    val v:Map[Int,Set[Atom]] = M.v.map

    fm match {
      case a: Atom    => v.getOrElse(t,Set()).contains(a) || M.B.contains(a)
      case Not(a)     => !(this ||- a)
      case And(a, b)  => (this ||- a) && (this ||- b)
      case Or(a, b)   => (this ||- a) || (this ||- b)
      case Impl(a, b) => !(this ||- a) || (this ||- b)
      case Diamond(a) => T.timePoints exists { u => M/S/u ||- a }
      case Box(a)     => T.timePoints forall { u => M/S/u ||- a }
      case At(u,a)    => (u in T) && (M/S/u ||- a)
      case Window(w,ch,x,a) => {
        val S1 = w(ch(S0,S),t,x)
        M/S1/t ||- a
      }

    }
  }
}

