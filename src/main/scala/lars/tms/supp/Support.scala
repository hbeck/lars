package lars.tms.supp

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.standard.StdProgram
import lars.tms.status.Labels

import scala.collection.immutable.HashMap

/**
  * Created by et on 20.11.16.
  */
case class Support(var suppP: Map[ExtendedAtom,Set[ExtendedAtom]] = new HashMap(),
                   var suppN: Map[ExtendedAtom,Set[ExtendedAtom]] = new HashMap(),
                   var suppAt: Map[ExtendedAtom,Set[ExtendedAtom]] = new HashMap()) {

  def supp(atom: ExtendedAtom) = suppP.getOrElse(atom,Set()) union suppN.getOrElse(atom,Set()) union suppAt.getOrElse(atom,Set())

  def updateP(P: StdProgram, L: Labels): Unit = {
    var updatedSupp: HashMap[ExtendedAtom,Set[ExtendedAtom]] = new HashMap()
    suppP.keys foreach {
      a => updatedSupp += (a -> SuppP(P,L,a))
    }
    suppP = updatedSupp
  }

  def updateN(P: StdProgram, L: Labels): Unit = {
    var updatedSupp: HashMap[ExtendedAtom,Set[ExtendedAtom]] = new HashMap()
    suppP.keys foreach {
      a => updatedSupp += (a -> SuppN(P,L,a))
    }
    suppN = updatedSupp
  }

  def updateAt(P: StdProgram, L: Labels): Unit = {
    var updatedSupp: HashMap[ExtendedAtom,Set[ExtendedAtom]] = new HashMap()
    suppP.keys foreach {
      a => updatedSupp += (a -> SuppAt(P,L,a))
    }
    suppAt = updatedSupp
  }
}

