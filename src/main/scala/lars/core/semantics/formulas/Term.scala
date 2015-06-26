package lars.core.semantics.formulas

/**
 * Created by hb on 5/31/15.
 */
abstract class Term

//Variable
case class V(s:String) extends Term // TODO equality local to formula/program
//Constant
case class C(s:String) extends Term


object Term {
  implicit def str2Term(s: String): Term = {
    if (s.charAt(0).isLower) C(s) else V(s)
  }
}
