package lars.core.semantics.programs

/**
  * Created by hb on 6/23/15.
  */
abstract class Program[R <: Rule](val rules:Set[R]) {

   override def toString = {
     val sb = new StringBuilder
     for (rule <- rules) {
       sb.append(rule).append(". ")
     }
     sb.toString
   }

   def apply(rules:Set[R]): Program[R]

}
