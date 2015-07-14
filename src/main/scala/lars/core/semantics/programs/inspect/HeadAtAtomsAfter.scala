package lars.core.semantics.programs.inspect

import lars.core.semantics.formulas.AtAtom
import lars.core.semantics.programs.general.GeneralProgram

/**
  * Created by hb on 7/14/15.
  */
object HeadAtAtomsAfter {

   def apply(P: GeneralProgram, t:Int): Set[AtAtom] = {
     HeadAtAtoms(P).filter( aa => (aa.t >= t))
   }

 }
