package lars.core.semantics.programs.general.inspect

import lars.core.semantics.programs.extatoms.AtAtom
import lars.core.semantics.programs.standard.StdProgram
import lars.core.semantics.programs.{Program, Rule}

/**
  * Created by hb on 7/14/15.
  */
object HeadAtAtomsAfter {

  def apply(P: StdProgram, t:Int): Set[AtAtom] = {
    HeadAtAtoms(P).filter( x => (x.t >= t))
  }

   def apply[R <: Rule](P: Program[R], t:Int): Set[AtAtom] = {
     HeadAtAtoms(P).filter( x => (x.t >= t))
   }

 }
