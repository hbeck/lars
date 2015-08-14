package lars.strat.alg

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.graph.quotient.Condensation
import lars.strat._
import org.scalatest.FunSuite

/**
 * Created by hb on 7/10/15.
 */
class TestStratumGraph extends FunSuite {

  object a extends Atom
  object b extends Atom
  object c extends Atom
  object d extends Atom
  object f extends Atom
  object g extends Atom
  object h extends Atom
  object i extends Atom
  object j extends Atom

  test("test1"){

    val a_c = e(a,c,geq)
    val b_a = e(b,a,geq)
    val b_d = e(b,d,geq)
    val d_c = e(d,c,grt)

    val nodes = Set[ExtendedAtom](d,b,c,a)
    val edges = Set[DepEdge](a_c,b_a,b_d,d_c)

    val depGraph = DepGraph(nodes,edges)
    val condensation = Condensation(depGraph)

    val quot = StratumGraph(depGraph,condensation).adjList

    println("test1: "+quot)

    val set_abd_c = Map(Set(c)->Set(),Set(a,b,d)->Set(Set(c)))
    val set_ac_bd = Map(Set(a,c)->Set(), Set(b,d)->Set(Set(a,c)))

    assert(quot == set_abd_c || quot == set_ac_bd)
  }
  test("test2"){

    val a_b = e(a,b,geq)
    val a_f = e(a,f,grt)
    val a_g = e(a,g,geq)
    val b_c = e(b,c,geq)
    val c_d = e(c,d,geq)
    val d_h = e(d,h,grt)
    val d_i = e(d,i,geq)
    val d_b = e(d,b,geq)
    val h_j = e(h,j,geq)
    val i_j = e(i,j,geq)
    val f_i = e(f,i,geq)
    val f_j = e(f,j,geq)
    val g_j = e(g,j,geq)

    val nodes = Set[ExtendedAtom](a,b,c,d,f,g,h,i,j)
//    println(nodes)
    val edges = Set[DepEdge](a_b,a_f,a_g,b_c,c_d,d_h,d_i,d_b,h_j,i_j,f_i,f_j,g_j)

    val depGraph = DepGraph(nodes,edges)

    val condensation = Condensation(depGraph)
    val quot = StratumGraph(depGraph,condensation).adjList

    println("test2: "+quot)

    for ((x,y) <- Seq((a,f),(d,h),(a,h))) {
      for (block <- quot.keys) {
        if (block.contains(x)) {
          assert(!block.contains(y))
        }
        if (block.contains(y)) {
          assert(!block.contains(x))
        }
      }
    }

    for (block <- quot.keys){
      if (block.contains(c)) {
        assert(block.contains(b))
        assert(block.contains(d))
      }
    }

  }

  test("test3"){
    object x1 extends Atom
    object x2 extends Atom
    object y1 extends Atom
    object y2 extends Atom
    object z1 extends Atom
    object z2 extends Atom
    object w extends Atom

    val x1_x2 = e(x1,x2,geq)
    val x2_x1 = e(x2,x1,geq)
    val x1_y1 = e(x1,y1,geq)
    val y1_z1 = e(y1,z1,grt)
    val y1_z2 = e(y1,z2,geq)
    val x2_y2 = e(x2,y2,geq)
    val w_y2  = e(w,y2,grt)

    val nodes = Set[ExtendedAtom](x1,x2,y1,y2,z1,z2,w)
//    println(nodes)
    val edges = Set[DepEdge](x1_x2,x2_x1,x1_y1,y1_z1,y1_z2,x2_y2,w_y2)

    val depGraph = DepGraph(nodes,edges)


    val condensation = Condensation(depGraph)
    val quot = StratumGraph(depGraph,condensation).adjList
    println("test3: "+quot)

    for (block <- quot.keys) {
      if (block.contains(w) || block.contains(z1)) {
        assert(block.size==1)
      } else {
        assert(block.size==5)
      }
    }
  }

  test("test4"){
    object x extends Atom

    val nodes = Set[ExtendedAtom](x)
    val edges = Set[DepEdge]()

    val depGraph = DepGraph(nodes,edges)

    val condensation = Condensation(depGraph)
    val quot = StratumGraph(depGraph,condensation).adjList
    //    println(quot)
    val set_x = Map(Set(x)->Set())

    assert(quot == set_x)
    println("test4: "+quot)
  }

  test("test5"){
    val a_b = e(a,b,grt)
    val a_c = e(a,c,grt)
    val c_d = e(c,d,geq)
    val b_d = e(b,d,geq)

    val nodes = Set[ExtendedAtom](d,b,c,a)
    val edges = Set[DepEdge](a_b,a_c,c_d,b_d)

    val depGraph = DepGraph(nodes,edges)

    val condensation = Condensation(depGraph)
    val quot = StratumGraph(depGraph,condensation).adjList

    println("test5: "+quot)
  }
}
