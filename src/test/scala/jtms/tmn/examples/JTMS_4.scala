package jtms.tmn.examples

import jtms.tmn.NodeValidation
import jtms.{in, out}
import org.scalatest.GivenWhenThen

/**
  * Created by FM on 05.02.16.
  */
class JTMS_4 extends JTMS with NodeValidation {

  val tmn = {
    val tmn = JTMS
    tmn.set(Set(E, B, D).to)
    tmn
  }

  "Node A" must behave like   nodeValidation(tmn, A) { validator =>
    validator.state(out)
    validator.SJ(None)
    validator.Supp(C)
    validator.SuppTrans(C, A)
    validator.Cons(B, C)
    validator.ACons(B, C)
    validator.AConsTrans(B, C, D, A, F)
    validator.Ant()
    validator.AntTrans()
  }

  "Node B" must behave like nodeValidation(tmn, B) { validator =>
    validator.state(in)
    validator.SJ(Some(j2))
    validator.Supp(A)
    validator.SuppTrans(A, C)
    validator.Cons(D)
    validator.ACons(D)
    validator.AConsTrans(D)
    validator.Ant(A)
    validator.AntTrans(A)
  }

  "Node C" must behave like nodeValidation(tmn, C) { validator =>
    validator.state(out)
    validator.SJ(None)
    validator.Supp(A)
    validator.SuppTrans(A, C)
    validator.Cons(A, D, F)
    validator.ACons(A, F)
    validator.AConsTrans(A, F, B, C, D)
    validator.Ant()
    validator.AntTrans()
  }

  "Node D" must behave like nodeValidation(tmn, D) { validator =>
    validator.state(in)
    validator.SJ(Some(j4a))
    validator.Supp(B)
    validator.SuppTrans(B, A, C)
    validator.Cons()
    validator.ACons()
    validator.AConsTrans()
    validator.Ant(B)
    validator.AntTrans(B, A)
  }

  "Node E" must behave like nodeValidation(tmn, E) { validator =>
    validator.state(in)
    validator.SJ(Some(j5))
    validator.Supp()
    validator.SuppTrans()
    validator.Cons(F)
    validator.ACons()
    validator.AConsTrans()
    validator.Ant()
    validator.AntTrans()
  }

  "Node F" must behave like nodeValidation(tmn, F) { validator =>
    validator.state(out)
    validator.SJ(None)
    validator.Supp(C)
    validator.SuppTrans(C, A)
    validator.Cons()
    validator.ACons()
    validator.AConsTrans()
    validator.Ant()
    validator.AntTrans()
  }
}
