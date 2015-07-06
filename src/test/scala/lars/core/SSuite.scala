package lars.core

import lars.core.semantics.formulas.Atom
import lars.core.semantics.streams.{Evaluation, S, Timeline}
import org.scalatest.FunSuite

/**
 * Created by hb on 5/18/15.
 */
class SSuite extends FunSuite {

   test("substream") {
     object a extends Atom
     val mp = Map(3 -> Set[Atom](a))
     val emp = Map[Int,Set[Atom]]()
     val s1 = S(Timeline(0, 5), Evaluation(mp))
     val s2 = S(Timeline(2, 4), mp) //using implicit Evaluation
     val s3 = S((0, 5), emp) //using also implicit Timeline
     assert(s1 <= s1)
     assert(s2 <= s1)
     assert(s3 <= s1)
   }

}
