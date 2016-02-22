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

  "The satisfiable result of clingo" should "be interpreted correctly and contain a, b" in {
    val clingo = ClingoWrapper()

    assert(clingo.parseResult("a b\nSATISFIABLE") == Some(Set("a", "b")))
  }
  it should "be interpreted correctly and only contain a" in {
    val clingo = ClingoWrapper()

    assert(clingo.parseResult("a\nSATISFIABLE") == Some(Set("a")))
  }

  "A not satisfiable result" should "return None" in {
    val clingo = ClingoWrapper()

    assert(clingo.parseResult("") == None)
  }

  "A satisfiable result with info/warnings" should "still be interpreted correctly and contain b,c" in {
    val clingo = ClingoWrapper()

    val result =
      """-:1:5-6: info: atom does not occur in any rule head:
  a

-:1:11-12: info: atom does not occur in any rule head:
  a

b c
SATISFIABLE"""

    assert(clingo.parseResult(result) == Some(Set("b", "c")))
  }
}
