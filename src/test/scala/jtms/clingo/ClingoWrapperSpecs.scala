package jtms.clingo

import org.scalatest.FlatSpec

/**
  * Created by FM on 21.02.16.
  */
class ClingoWrapperSpecs extends FlatSpec {

  "The clingo wrapper" can "be initialized by parsing the clingo version" in {
    val clingo = ClingoWrapper()

    assert(clingo.clingoVersion == "4.5.3")
  }

  it can "parse the clingo version string" in {
    assert(ClingoWrapper.parseVersion("clingo version 4.5.3") == "4.5.3")
  }

  it should "throw on non matching version" in {
    intercept[IllegalArgumentException] {
      ClingoWrapper.parseVersion("foo var 1.2.3")
    }
  }

  it should "have a factory with an instance of Process" in {
    val clingo = ClingoWrapper()

    assert(clingo.clingoProcess.isInstanceOf[scala.sys.process.ProcessBuilder])
  }

  it should "return the model for a simple ASP program" in {
    val clingo = ClingoWrapper()

    assert(clingo.run("a. b :- a.") == "a b\nSATISFIABLE")
 }

}
