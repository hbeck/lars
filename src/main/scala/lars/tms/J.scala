package lars.tms

/**
 * Created by hb on 6/25/15.
 */
case class J(I:Set[Node], O:Set[Node], n:Node) {

  def fires (m:TMSModel): Boolean = {
    (I forall m.in) && (O forall m.out)
  }

}
