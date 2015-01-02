package lars.exp

import lars.exp.Formulas._

import scala.collection.immutable.Set

/**
 * Created by hb on 1/2/15.
 */
object Experiments {
  def main(args: Array[String]) {

    val a = Atom("a");
    val b = Atom("b");
    val c = Atom("c");

    val fm1 = Or(And(a,b),Not(c))
    val fm2 = (a.and(b)).or(c.not())
    val fm3 = (a and b) or (c not)
    val fm4 = (a and b) or not(c)

    val emptySet = Set[Atom]()
    val I0 = Interpretation(emptySet)

    for (fm <- List(fm1,fm2,fm3,fm4)) {
      println("I0 |= " + fm + "?: " + (I0 |= fm))
    }

  }
}


