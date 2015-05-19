package lars.core

import lars.core.Formulas.Atom
import lars.core.LStreams.{Evaluation, LStream, Timeline}
import org.scalatest.FunSuite

/**
 * Created by hb on 5/18/15.
 */
class LStreamSuite extends FunSuite {

   test("substream") {
     val m = Map(3 -> Set(Atom("a")))
     val em = Map[Int,Set[Atom]]()
     val S1 = LStream(Timeline(0, 5), Evaluation(m))
     val S2 = LStream(Timeline(2, 4), m) //using implicit Evaluation
     val S3 = LStream((0, 5), em) //using also implicit Timeline
     assert(S1 <= S1)
     assert(S2 <= S1)
     assert(S3 <= S1)
   }

}
