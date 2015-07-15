package lars.core.semantics.programs

import lars.core.semantics.structure.{Mt, M}

/**
 * Created by hb on 7/15/15.
 */
object Reduct {

  def apply[R <: Rule, Pr <: Program[R]](P: Pr, m:M, t:Int): Pr = {
    reduct(P,Mt(m,t))
  }

  def reduct[R <: Rule, Pr <: Program[R]](P: Pr, mt:Mt): Pr = {
    val rs = P.rules.filter(mt |= _.body)
    P(rs)
  }

}
