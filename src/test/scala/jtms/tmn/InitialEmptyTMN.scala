package jtms.tmn

import jtms.{in, Justification, TMN, Node}
import org.scalatest.{BeforeAndAfter}

/**
  * Created by FM on 05.02.16.
  */
class InitialEmptyTMN extends TMNSpec with BeforeAndAfter {

  val assumptionA = Assumption("A")
  val tmn = EmptyTMN

  before {
    tmn.update(assumptionA)
  }

  "An empty TMN" should "have no Justifications and no status values set" in {
    val empty = EmptyTMN
    assert(empty.J.size == 0)
    assert(empty.status.size == 0)
  }

  "A TMN with one assumption" should "generate a status entry after update" in {
    assert(tmn.status.isEmpty == false)
  }

  it should "contain one entry with status in" in {
    assert(tmn.status.filter(_._2 == in).size == 1)
  }

  it should "contain one Justification" in {
    assert(tmn.J.size == 1)
  }

}
