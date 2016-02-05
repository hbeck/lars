package jtms

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{HashMap, HashSet, Map, Set}

/**
  * truth maintenance network
  * Created by hb on 12/22/15.
  */
class TMN(val N: collection.immutable.Set[Node], val J: Set[Justification] = Set()) {

  val Cons: Map[Node, Set[Node]] = new HashMap[Node, Set[Node]]
  val Supp: Map[Node, Set[Node]] = new HashMap[Node, Set[Node]]
  val SJ: Map[Node, Option[Justification]] = new HashMap[Node, Option[Justification]]
  val status: Map[Node, Status] = new HashMap[Node, Status]

  for (n <- N) {
    init(n)
  }
  for (j <- J) {
    for (m <- j.I union j.O) {
      Cons(m) += j.n
    }
  }

  def init(n:Node) ={
    if(!status.isDefinedAt(n)) status(n) = out
    if(!Cons.isDefinedAt(n))Cons(n) = new HashSet[Node]()
    if(!Supp.isDefinedAt(n))Supp(n) = new HashSet[Node]()
    if(!SJ.isDefinedAt(n)) SJ(n) = None
  }

  /** @return true if M is admissible **/
  def set(M: List[Node]): Boolean = {
    for (i <- 0 to M.size - 1) {
      val j: Option[Justification] = findSJ(M, i)
      if (j.isEmpty) {
        return false
      }
      setIn(j.get)
    }
    for (n <- N.diff(M.toSet)) {
      setOut(n)
    }
    true
  }

  /** takes node at list M index idx and tries to find a valid justification
    * that is founded wrt indexes 0..idx-1
    */
  def findSJ(M: List[Node], idx: Int): Option[Justification] = {
    val n = M(idx)
    val MSub = M.take(idx).toSet
    Jn(n).find(j => j.I.subsetOf(MSub) && j.O.intersect(M.toSet).isEmpty)
  }

  def update(j: Justification): Unit = {

    def n = j.n //alias

    //update structure
    J += j
    for (m <- j.I union j.O) {
      Cons(m) += n
    }

    init(n)

    //if conclusion was already drawn, we are done
    if (status(n) == in) {
      return
    }

    //otherwise, we are done, if the new justification is not valid in M, i.e.,
    //n does not need to be concluded
    val spoiler: Option[Node] = findSpoiler(j)
    if (spoiler.isDefined) {
      Supp(n) += spoiler.get
      return
    }

    if (ACons(n).isEmpty) {
      //then we can treat n independently
      setIn(j)
      return
    }

    val L = AConsTrans(n) + n
    setUnknown(L)

    setConsequences(L)

    chooseAssignments(L)
  }

  def Jn(n: Node) = J.filter(_.n == n)

  //ACons(n) = {x ∈ Cons(n) | n ∈ Supp(x)}
  def ACons(n: Node): Set[Node] = Cons(n).filter(Supp(_).contains(n))

  def AConsTrans(n: Node): Set[Node] = trans(ACons, n)

  def setIn(j: Justification) = {
    status(j.n) = in
    Supp(j.n) =  (j.I union j.O).to
    SJ(j.n) = Option(j)
  }

  def setOut(n: Node) = {
    status(n) = out
    Supp(n) = Jn(n).map(findSpoiler(_).get)
    SJ(n) = None
  }

  def findSpoiler(j: Justification): Option[Node] = {
    if (math.random < 0.5) {
      val opt = j.I.find(status(_) == out)
      if (opt.isDefined) {
        return opt
      } else {
        return j.O.find(status(_) == in)
      }
    } else {
      val opt = j.O.find(status(_) == in)
      if (opt.isDefined) {
        return opt
      } else {
        return j.I.find(status(_) == out)
      }
    }
  }

  def setUnknown(n: Node): Unit = {
    status(n) = unknown
    Supp(n).clear()
    SJ(n) = None
  }

  def setUnknown(L: Set[Node]): Unit = L.foreach(setUnknown(_))

  def setConsequences(L: mutable.Set[Node]): Unit = {
    for (n <- L) {
      setConsequences(n)
    }
  }

  def setConsequences(n: Node): Unit = {
    if (status(n) == unknown) {
      val jn: Set[Justification] = Jn(n)
      val j: Option[Justification] = jn.find(foundedValid)
      if (j.isDefined) {
        setIn(j.get)
        setConsequences(unknownCons(n))
      } else if (jn.forall(foundedInvalid)) {
        setOut(n)
        setConsequences(unknownCons(n))
      }
    }
  }

  def chooseAssignments(L: mutable.Set[Node]): Unit = {
    for (n <- L) {
      chooseAssignments(n)
    }
  }

  def chooseAssignments(n: Node): Unit = {
    if (status(n) == unknown) {
      val jn: Set[Justification] = Jn(n)
      val j: Option[Justification] = jn.find(unfoundedValid)
      if (j.isDefined) {
        val aCons = ACons(n)
        if (aCons.isEmpty) {
          setIn(j.get)
          j.get.O.foreach(status(_) = out)
          chooseAssignments(unknownCons(n))
        } else {
          aCons += n
          for (m <- aCons) {
            status(m) = unknown
            chooseAssignments(m)
          }
        }
      } else {
        //all jn are unfounded invalid. in particular, for every j in jn, some node in j.I is unknown
        status(n) = out
        for (h <- jn) {
          val m = h.I.find(status(_) == unknown).get
          status(m) = out
        }
        setOut(n)
        chooseAssignments(unknownCons(n))
      }
    }
  }

  def unknownCons(n: Node) = Cons(n).filter(status(_) == unknown)

  //TODO what about the ordering?
  def foundedValid(j: Justification): Boolean = {
    j.I.forall(status(_) == in) && j.O.forall(status(_) == out)
  }

  def foundedInvalid(j: Justification): Boolean = {
    j.I.exists(status(_) == out) || j.O.exists(status(_) == in)
  }

  def unfoundedValid(j: Justification): Boolean = {
    j.I.forall(status(_) == in) && !j.O.exists(status(_) == in) //&& j.O.exists(status(_)==unknown)
  }

  def trans[T](f: T => Set[T], t: T): Set[T] = {
    trans(f)(Set(t))
  }

  @tailrec
  final def trans[T](f: T => Set[T])(s: Set[T]): Set[T] = {
    val next: Set[T] = s.flatMap(f)
    if (s == next) {
      return s
    }
    trans(f)(next)
  }

}
