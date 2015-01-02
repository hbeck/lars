package lars.exp

object Formulas {

  /**
   * Created by hb on 1/2/15.
   */
  abstract class Formula {
    def and(f: Formula): Formula = And(this, f)
    def or(f: Formula): Formula = Or(this, f)
    def implies(f: Formula): Formula = Impl(this, f)
    def not(): Formula = Not(this)
  }

  case class Atom(n: String) extends Formula
  case class Not(a: Formula) extends Formula
  case class And(a: Formula, b: Formula) extends Formula
  case class Or(a: Formula, b: Formula) extends Formula
  case class Impl(a: Formula, b: Formula) extends Formula
  case class D(a: Formula) extends Formula
  case class B(a: Formula) extends Formula
  case class At(a: Formula, t: Int) extends Formula

  def not(f: Formula) = Not(f)

}

