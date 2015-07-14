package lars.core.semantics.programs

import lars.core.semantics.structure.{M, Mt}

/**
  * Created by hb on 6/23/15.
  */
case class Program[R <: Rule](rules:Set[R]) {
   def reduct(m:M, t:Int): Program[R] = {
     reduct(Mt(m,t))
   }
   def reduct(mt:Mt): Program[R] = {
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
