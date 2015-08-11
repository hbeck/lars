package lars.core.semantics.programs

import lars.core.semantics.programs.general.inspect._
import lars.core.semantics.streams.S
import lars.core.semantics.structure.IsAnswerStream

/**
 * Created by hb on 7/13/15.
 */
object AS {

  //naive implementation for the moment
  def apply[R <: Rule](P: Program[R], D: S, t: Int): Set[S] = {
    val AInt = IntensionalAtoms(P)
    val headOrdinaryAtoms = HeadOrdinaryAtoms(P)
    val atAtomsInHead = Map2Time(HeadAtAtomsAfter(P,t))
    val temporallyQuantifiedInHead = TemporallyQuantifiedInHead(P)
    //1) create maximal extension
    var I = S(D.T)
    for (a <- AInt) {
      if (temporallyQuantifiedInHead.contains(a)) { //place everywhere
        //TODO restrict to window instead of the entire timeline D.T
         for (u <- D.T.lower to D.T.upper) {
           I = I + (u -> a)
         }
      } else {
        if (headOrdinaryAtoms.contains(a)) { //place only now
          I = I + (t -> a)
        }
        if (atAtomsInHead.contains(a)) { //place only at given times
          for (u <- atAtomsInHead(a)) {
            I = I + (u -> a)
          }
        }
      }
    }
    //2) iterate subsets
    var cntAll=0
    var answerStreams = collection.immutable.HashSet[S]()
    for (addS <- I.substreams()) { //todo iterate bottom up
      cntAll += 1
      val candidateS = D ++ addS
      if (IsAnswerStream(candidateS,P,D,t)) {
        answerStreams += candidateS
        //TODO: skip additions to this model
      }
    }
    //println("tested "+cntAll+" models")
    answerStreams.toSet
  }

}
