package lars.ijcai15

import lars.core.semantics.formulas._
import lars.core.semantics.streams.{Evaluation, S, Timeline}
import lars.core.windowfn.timebased.TimeBasedWindow
import org.scalatest.FunSuite

/**
 * Created by hb on 5/19/15.
 */
class ExamplesIJCAI15 extends FunSuite {

  object busG extends Atom
  object tramB extends Atom
  object expBusM extends Atom
  object expTrM extends Atom
  object on extends Atom
  object request extends Atom
  object takeBusM extends Atom
  object takeTrM extends Atom
  object jam extends Atom

  def m(i:Double) = (i*10*60).toInt

  val T = Timeline(0,m(50))
  val v = Evaluation(Map(m(37.2) -> Set(busG), m(39.1) -> Set(tramB)))
  val D = S(T,v)
  //
  def w = TimeBasedWindow

  test("ex3") {
    val t = m(39.7)
    assert(t == 23820)
    val Sp = w(D,t,m(3))
    assert(Sp.T == Timeline(m(36.7),m(39.7)))
    assert(Sp.size == 2)
  }

  test("ex4") {
    val B:Set[Atom] = Set()
    val M = D.toStructure()
    val w3 = w.toOp(m(3))
    val f = w3(At(m(37.2),busG))
    for (t <- m(37.2) to m(40.2)) {
      assert(M/t |= f)
    }
    assert(false == (M/(m(37.2)-1) |= f))
    assert(false == (M/(m(40.2)+1) |= f))
  }

}
