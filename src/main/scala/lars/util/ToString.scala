package lars.util

/**
 * Created by hb on 11/14/15.
 */
object ToString {

  def apply[T](set:Set[T]):String = {
    set.size match {
      case 0 => "{}"
      case 1 => "{" + set.head + "}"
      case _ => "{" + set.map(x => x.toString).reduce((s1,s2)=>s1+", "+s2) + "}"
    }
  }

}
