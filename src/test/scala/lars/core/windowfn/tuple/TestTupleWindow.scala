package lars.core.windowfn.tuple

import lars.core.semantics.formulas.Atom
import lars.core.semantics.streams.{S, Timeline}
import lars.core.windowfn.tuple.TupleWindow
import org.scalatest.FunSuite

import scala.collection.immutable.Map

/**
 * Created by hb on 7/14/15.
 */
class TestTupleWindow extends FunSuite {

  object x extends Atom
  object y extends Atom

  val T = Timeline(0,5)
  val s1 = S(T)
  val s2 = s1 + (3->x)
  val s3 = s1 + (2->x)
  val s4 = s1 + (4->x)
  val s5 = s2 + (1->x) + (5->x)
  val s7 = s2 ++ s4 + (3->y)
  val s8 = s7 + (1->x) + (5->x)
  val s6 = s8 + (1->y) + (5->y) - (3->y)

 /* val stream = Map[Int,S](
    1 -> s1, 2 -> s2, 3 -> s3, 4 -> s4,
    5 -> s5, 6 -> s6, 7 -> s7, 8 -> s8
  )*/

  test("test (1,0)") {
    val w = TupleWindow.fix(1)

    assert(w(s1,3) == (s1|Timeline(0,3)))
    assert(w(s2,3) == (s2|Timeline(3,3)))
    assert(w(s3,3) == (s3|Timeline(2,3)))
    assert(w(s4,3) == (s4|Timeline(0,3)))
    assert(w(s5,3) == (s5|Timeline(3,3)))
    assert(w(s6,3) == (s6|Timeline(3,3)))

    val s7_x = S(Timeline(3,3),3->x)
    val s7_y = S(Timeline(3,3),3->y)


    assert(w(s7,3) == s7_x || w(s7,3) == s7_y)
    assert(w(s8,3) == s7_x || w(s8,3) == s7_y)
  }

    test("test (2,0)") {
      val w = TupleWindow.fix(2)

      assert(w(s1,3) == (s1|Timeline(0,3)))
      assert(w(s2,3) == (s2|Timeline(0,3)))
      assert(w(s3,3) == (s3|Timeline(0,3)))
      assert(w(s4,3) == (s4|Timeline(0,3)))
      assert(w(s5,3) == (s5|Timeline(1,3)))

      val s6_x = S(Timeline(1,3),1->x) + (3->x)
      val s6_y = S(Timeline(1,3),1->y) + (3->x)

      assert(w(s6,3) == s6_x || w(s6,3) == s6_y)
      assert(w(s7,3) == (s7|Timeline(3,3)))
      assert(w(s8,3) == (s8|Timeline(3,3)))
    }


    test("test (0,2)") {
      val w = TupleWindow.fix(0,2)

      assert(w(s1,3) == (s1|Timeline(4,5)))
      assert(w(s2,3) == (s2|Timeline(4,5)))
      assert(w(s3,3) == (s3|Timeline(4,5)))
      assert(w(s4,3) == (s4|Timeline(4,5)))
      assert(w(s5,3) == (s5|Timeline(4,5)))

      val s6_x = S(Timeline(4,5),5->x) + (4->x)
      val s6_y = S(Timeline(4,5),5->y) + (4->x)

      assert(w(s6,3) == s6_x || w(s6,3) == s6_y)

      assert(w(s7,3) == (s7|Timeline(4,5)))
      assert(w(s8,3) == (s8|Timeline(4,5)))


    }

    test("test (1,1)") {
      val w = TupleWindow.fix(1,1)

      assert(w(s1,3) == (s1|Timeline(0,5)))
      assert(w(s2,3) == (s2|Timeline(3,5)))
      assert(w(s3,3) == (s3|Timeline(2,5)))
      assert(w(s4,3) == (s4|Timeline(0,4)))
      assert(w(s5,3) == (s5|Timeline(3,5)))
      assert(w(s6,3) == (s6|Timeline(3,4)))

      val s7_x = S(Timeline(3,4),3->x) + (4->x)
      val s7_y = S(Timeline(3,4),3->y) + (4->x)


      assert(w(s7,3) == s7_x || w(s7,3) == s7_y)
      assert(w(s8,3) == s7_x || w(s8,3) == s7_y)
    }


}
