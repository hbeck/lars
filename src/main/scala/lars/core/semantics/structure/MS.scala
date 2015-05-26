package lars.core.semantics.structure

import lars.core.semantics.streams.S

/**
 * Created by hb on 5/26/15.
 */
//M,S
case class MS(m: M, s: S) {
  def /(t: Int) = MSt(m,s,t)
}
