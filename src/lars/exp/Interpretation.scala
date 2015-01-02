package lars.exp

import Formulas._

import scala.collection.immutable.Set

/**
 * Created by hb on 1/2/15.
 */
case class Interpretation(v:Set[Atom]) {

  def |= (fm:Formula):Boolean = {
    fm match {
      case a:Atom => v contains a
      case Not(a) => ! (this |= a)
      case And(a,b) => (this |= a) && (this |= b)
      case Or(a,b) => (this |= a) || (this |= b)
      case Impl(a,b) => !(this |= a) || (this |= b)
    }
  }

  override def toString():String = {
    v.toString
  }

}
