package jtms

import scala.Predef
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{HashMap, HashSet, Map, Set}

/**
  * truth maintenance network
  * Created by hb on 12/22/15.
  */
class TMN(var N: collection.immutable.Set[Node], var J: Predef.Set[Justification] = Predef.Set()) {

  val Cons: Map[Node, Predef.Set[Node]] = new HashMap[Node, Predef.Set[Node]]
  val Supp: Map[Node, Predef.Set[Node]] = new HashMap[Node, Predef.Set[Node]]
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

  def init(n: Node) = {
    if (!status.isDefinedAt(n)) status(n) = out
    if (!Cons.isDefinedAt(n)) Cons(n) = Predef.Set[Node]()
    if (!Supp.isDefinedAt(n)) Supp(n) = Predef.Set[Node]()
    if (!SJ.isDefinedAt(n)) SJ(n) = None
  }

  /** @return true if M is admissible **/
  def set(M: Predef.Set[Node]): Boolean = {
    val m = M.toList
    for (i <- 0 to M.size - 1) {
      val j: Option[Justification] = findSJ(m, i)
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

  def getModel() = {
    status.filter(_._2 == in).map(_._1).toSet
  }

  /** takes node at list M index idx and tries to find a valid justification
    * that is founded wrt indexes 0..idx-1
    */
  def findSJ(M: List[Node], idx: Int): Option[Justification] = {
    val n = M(idx)
    val MSub = M.take(idx).toSet
    val justifications = Jn(n).filter(j => j.I.subsetOf(MSub) && j.O.intersect(M.toSet).isEmpty)
    selectJustification(justifications)
  }

  def update(j: Justification): Predef.Set[Node] = {

    val n = j.n //alias

    //update structure
    J += j
    for (m <- j.I union j.O) {
      Cons(m) += n
    }

    init(n)

    //if conclusion was already drawn, we are done
    if (status(n) == in) {
      doDDB()
      return scala.collection.immutable.Set()
    }

    //otherwise, we are done, if the new justification is not valid in M, i.e.,
    //n does not need to be concluded
    val spoiler: Option[Node] = findSpoiler(j)
    if (spoiler.isDefined) {
      Supp(n) += spoiler.get
      doDDB
      return scala.collection.immutable.Set()
    }

    if (ACons(n).isEmpty) {
      //then we can treat n independently
      setIn(j)
      doDDB
      // TODO (CF): Missing to add n to M (M = M + n)?
      return scala.collection.immutable.Set()
    }

    val L = AffectedNodes(n)

    updateNodes(L)
  }

  def updateNodes(L: Predef.Set[Node]): Predef.Set[Node] = {

    def stateOfNodes(nodes: Predef.Set[Node]) = nodes.map(n => (n, status(n))).toList

    val oldState = stateOfNodes(L)

    setUnknown(L)

    setConsequences(L)

    chooseAssignments(L)

    doDDB

    val newState = stateOfNodes(L)

    val diffState = oldState.diff(newState)

    diffState.map(_._1).toSet
  }

  def remove(j: Justification) = {

    val contradictionJustifications = J.filter(_.isInstanceOf[ContradictionJustification])

    val L = AffectedNodes(j.n) ++ contradictionJustifications.flatMap(x => AffectedNodes(x.n))

    def removeJustification(j: Justification) = {
      for (m <- j.I union j.O) {
        Cons(m) -= j.n
      }

      J -= j
    }

    removeJustification(j)

    contradictionJustifications.foreach(removeJustification)

    this.updateNodes(L)
  }


  def doDDB() = {
    val model = getModel()

    for (n <- model) {
      if (Ncont.contains(n) && status(n) == in)
        DDB(n)
    }
  }

  def DDB(n: Node) = {
    val assumptions = MaxAssumptions(n)

    if (assumptions.isEmpty)
      throw new RuntimeException("We have an unsolveable contradiction for node " + n)

    // TODO: Ordering + Selection?
    val n_a = selectJustification(assumptions).get

    // TODO: Ordering + Selection?
    // (we pick currently only the first O)
    val n_star = selectNode(n_a.O).get

    val j_cont = J_cont(assumptions.map(_.n))

    val I_cont = j_cont.flatMap(_.I)
    val O_cont = j_cont.flatMap(_.O) - n_star;

    val justification = new ContradictionJustification(I_cont, O_cont, n_star)

    update(justification)
  }

  def J_cont(nodes: Predef.Set[Node]) = {
    SJ.filterKeys(nodes.contains(_)).values.map(_.get).toSet
  }

  def Jn(n: Node) = J.filter(_.n == n)

  //ACons(n) = {x ∈ Cons(n) | n ∈ Supp(x)}
  def ACons(n: Node): Predef.Set[Node] = Cons(n).filter(Supp(_).contains(n))

  def AConsTrans(n: Node) = trans(ACons, n)

  def SuppTrans(n: Node) = trans(Supp, n)

  def Ant(n: Node): Predef.Set[Node] = {
    if (status(n) == in)
      return Supp(n)
    return Predef.Set()
  }

  def AntTrans(n: Node) = trans(Ant, n)

  def AffectedNodes(n: Node) = AConsTrans(n) + n


  def MaxAssumptions(n: Node): Predef.Set[Justification] = {
    val assumptions = Set[Justification]()

    def asAssumption(n: Node) = SJ(n).filterNot(_.O.isEmpty)

    if (Ncont.contains(n)) {
      val assumptionsOfN = AntTrans(n).map(asAssumption).filter(_.isDefined).map(_.get)

      for (a <- assumptionsOfN) {
        val otherAssumptions = assumptionsOfN - a

        val allOtherAssumptions = otherAssumptions.flatMap(x => AntTrans(x.n)).toSet

        if (!allOtherAssumptions.contains(a.n))
          assumptions.add(a)
      }
    }

    assumptions.toSet
  }

  def setIn(j: Justification) = {
    status(j.n) = in
    Supp(j.n) = j.I union j.O
    SJ(j.n) = Option(j)
  }

  def setOut(n: Node) = {
    status(n) = out
    Supp(n) = Jn(n).map(findSpoiler(_).get)
    SJ(n) = None
  }

  def findSpoiler(j: Justification): Option[Node] = {
    if (math.random < 0.5) {
      val opt = selectNode(j.I.filter(status(_) == out))
      if (opt.isDefined) {
        return opt
      } else {
        return selectNode(j.O.filter(status(_) == in))
      }
    } else {
      val opt = selectNode(j.O.filter(status(_) == in))
      if (opt.isDefined) {
        return opt
      } else {
        return selectNode(j.I.filter(status(_) == out))
      }
    }
  }

  def setUnknown(n: Node): Unit = {
    status(n) = unknown
    Supp(n) = Predef.Set()
    SJ(n) = None
  }

  def setUnknown(L: Predef.Set[Node]): Unit = L.foreach(setUnknown(_))

  def setConsequences(L: Predef.Set[Node]): Unit = {
    for (n <- L) {
      setConsequences(n)
    }
  }

  def setConsequences(n: Node): Unit = {
    if (status(n) == unknown) {
      val jn = Jn(n)
      val j: Option[Justification] = selectJustification(jn.filter(foundedValid))
      if (j.isDefined) {
        setIn(j.get)
        setConsequences(unknownCons(n))
      } else if (jn.forall(foundedInvalid)) {
        setOut(n)
        setConsequences(unknownCons(n))
      }
    }
  }

  def chooseAssignments(L: Predef.Set[Node]): Unit = {
    for (n <- L) {
      chooseAssignments(n)
    }
  }

  def chooseAssignments(n: Node): Unit = {
    if (status(n) == unknown) {
      val jn = Jn(n)
      val j: Option[Justification] = selectJustification(jn.filter(unfoundedValid))
      if (j.isDefined) {
        val aCons = ACons(n)
        if (aCons.isEmpty) {
          setIn(j.get)
          j.get.O.filter(status(_) == unknown).foreach(status(_) = out)
          chooseAssignments(unknownCons(n))
        } else {
          //          aCons += n
          for (m <- (aCons + n)) {
            status(m) = unknown
            chooseAssignments(m)
          }
        }
      } else {
        //all jn are unfounded invalid. in particular, for every j in jn, some node in j.I is unknown
        status(n) = out
        for (h <- jn) {
          val m = selectNode(h.I.filter(status(_) == unknown))
          // TODO: this might be needed because of the non existing ordering
          // We usually can expect to always have a justification (if ordering is correct)
          m.foreach(status(_) = out)
        }
        setOut(n)
        chooseAssignments(unknownCons(n))
      }
    }
  }

  def Ncont = N.filter(_.isInstanceOf[ContradictionNode])
  def unknownCons(n: Node) = Cons(n).filter(status(_) == unknown)

  def selectNode(nodes: scala.Predef.Set[Node]): Option[Node] = {
    if (nodes.isEmpty)
      return None

    //TODO what about the ordering?
    Some(nodes.head)
  }

  def selectJustification(justifications: Predef.Set[Justification]): Option[Justification] = {
    if (justifications.isEmpty)
      return None

    //TODO what about the ordering?
    Some(justifications.head)
  }


  def foundedValid(j: Justification): Boolean = {
    j.I.forall(status(_) == in) && j.O.forall(status(_) == out)
  }

  def foundedInvalid(j: Justification): Boolean = {
    j.I.exists(status(_) == out) || j.O.exists(status(_) == in)
  }

  def unfoundedValid(j: Justification): Boolean = {
    j.I.forall(status(_) == in) && !j.O.exists(status(_) == in) //&& j.O.exists(status(_)==unknown)
  }

  def trans[T](f: T => Predef.Set[T], t: T): Predef.Set[T] = {
    trans(f)(f(t))
  }

  @tailrec
  final def trans[T](f: T => Predef.Set[T])(s: Predef.Set[T]): Predef.Set[T] = {
    val next = s.flatMap(f)
    val nextSet = next ++ s
    if (s == nextSet || next.isEmpty) {
      return s
    }
    trans(f)(nextSet)
  }

}
