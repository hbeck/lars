package lars.core.semantics.programs

import lars.core.semantics.programs.general.GeneralProgram
import lars.core.semantics.programs.inspect._
import lars.core.semantics.streams.S

/**
 * Created by hb on 7/13/15.
 */
object AS {

  //naive implementation for the moment
  def apply(P: Program, D: S, t: Int): Set[S] = {
    val AInt = IntensionalAtoms(P)
    val atomsInHead = HeadOrdinaryAtoms(P)
    val atAtomsInHead = Map2Time(HeadAtAtomsAfter(P,t))
    //1) create maximal extension
    var I = S(D.T)
    for (a <- AInt) {
      if (atomsInHead.contains(a)) { //place only now
        I = I + (t -> a)
      } else {
        //place only at given times
        for (u <- atAtomsInHead(a)) {
          I = I + (u -> a)
        }
      }
    }
    //2) iterate subsets
    var cntAll=0
    var answerStreams = collection.immutable.HashSet[S]()
    for (addS <- I.substreams()) { //todo iterate bottom up
      cntAll += 1
      val candidateS = D ++ addS
      if (candidateS.isAnswerStream(P,D,t)) {
        answerStreams += candidateS
        //TODO: skip additions to this model
      }
    }
    //println("tested "+cntAll+" models")
    answerStreams.toSet
  }

}
