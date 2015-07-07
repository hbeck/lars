package lars.core.semantics.programs

import lars.core.semantics.structure.{M, Mt}

/**
 * Created by hb on 6/23/15.
 */
case class Program(rules:Set[Rule]) {
  def reduct(m:M, t:Int): Program = {
    reduct(Mt(m,t))
  }
  def reduct(mt:Mt): Program = {
    val rs = this.rules.filter(mt |= _.body)
    Program(rs)
  }
  override def toString = {
    val sb = new StringBuilder
    for (rule <- rules) {
      sb.append(rule).append(". ")
    }
    sb.toString
  }
}
