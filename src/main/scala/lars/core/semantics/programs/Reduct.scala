package lars.core.semantics.programs

import lars.core.semantics.programs.general.GeneralProgram
import lars.core.semantics.programs.standard.StdProgram
import lars.core.semantics.structure.{M, Mt}

/**
 * Created by hb on 7/15/15.
 */
object Reduct {

  def apply(P: StdProgram, m:M, t:Int): StdProgram = {
    reduct(P,Mt(m,t))
  }

  def reduct(P: StdProgram, mt:Mt): StdProgram = {
    val rs = P.rules.filter(mt |= _.body)
    P(rs)
  }

  def apply(P: GeneralProgram, m:M, t:Int): GeneralProgram = {
    reduct(P,Mt(m,t))
  }

  def reduct(P: GeneralProgram, mt:Mt): GeneralProgram = {
    val rs = P.rules.filter(mt |= _.body)
    P(rs)
  }

  def apply[R <: Rule](P: Program[R], m:M, t:Int): Program[R] = {
    reduct(P,Mt(m,t))
  }

  def reduct[R <: Rule](P: Program[R], mt:Mt): Program[R] = {
    val rs = P.rules.filter(mt |= _.body)
    P(rs)
  }

}
