package lars.core.windowfn.partition

import lars.core.semantics.formulas.{Term, Atom}
import lars.core.semantics.streams.{S, Timeline}
import lars.core.windowfn.tuple.{TupleWindowParameters, TupleWindowFixedParams}
import org.scalatest.FunSuite


/**
  * Created by et on 7/15/15.
  */
class TestPartitionWindow extends FunSuite {

  case class tram(vehicleId:Term, station: Term) extends Atom
  case class exp(vehicleId:Term, station: Term) extends Atom


  /*Test from AAAI extended 2015*/
  def idx(a: Atom): Int = a match {
    case tram(vehicleId, station) => vehicleId.toString.charAt(2).toInt     //TODO better way to convert vehicleID into Integer
    case exp(vehicleId, station) => vehicleId.toString.charAt(2).toInt + 2  //NOTE: numbers are interpreted as capital letters
    case _ => 0
  }

  def n(i: Int): TupleWindowFixedParams = i match {
    case 0 => TupleWindowFixedParams(TupleWindowParameters(0,0))
    case _ => TupleWindowFixedParams(TupleWindowParameters(1,0))
  }

  val tr1 = tram("1","b")
  val tr11 = tram("1","m")
  val tr2 = tram("2", "h")
  val tr22 = tram("2", "m")
  val b1 = exp("3","b")
  val b11 = exp("3","s")
  val b2 = exp("4","m")


  val T = Timeline(0,50)
  val s = S(T) + (36->tr1) + (36->b1) + (40->tr2) + (40->b11) + (43->tr22) + (44->tr11) + (45->b2)

  test("test exp12") {
    val w = PartitionWindow.fix(idx, n)
    println(w(s,45))
    assert(w(s,45) == (S(T) + (40->b11) + (43->tr22) + (44->tr11) + (45->b2)))
  }
}
