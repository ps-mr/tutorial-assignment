package minCostFlow

import org.scalatest._

class RelaxationSpec extends FlatSpec {
  import Relaxation._

  "Relaxation" should "handle the empty instance" in {
    val flow = computeFlow (
      supply   = Vector.empty,
      edges    = Vector.empty,
      cost     = Vector.empty,
      capacity = Vector.empty
    )
    assert(flow.isEmpty)
  }

  it should "handle x -> y" in {
    val flow = computeFlow (
      supply   = Vector(10, -10),
      edges    = Vector((0, 1)),
      cost     = Vector(10),
      capacity = Vector(10)
    )
    assert(flow == Seq(10))
  }

  it should "handle x -> y -> z" in {
    val flow = computeFlow (
      supply   = Vector(10, 0, -10),
      edges    = Vector((0, 1), (1, 2)),
      cost     = Vector(10, 10),
      capacity = Vector(10, 10)
    )
    assert(flow == Seq(10, 10))
  }

  it should "handle a triangle with the long path cheaper" in {
    val flow = computeFlow (
      supply   = Vector(1, 0, -1),
      edges    = Vector((0, 2), (0, 1), (1, 2)),
      cost     = Vector(20, 5, 5),
      capacity = Vector(1, 1, 1)
    )
    assert(flow == Seq(0, 1, 1))
  }

  it should "handle the example in Fernandez's slides for Uni Heidelberg" in {
    // http://www.iwr.uni-heidelberg.de/groups/comopt/teaching/ws10/effAlgI/ueb/fernandez_mincostflow.pdf
    // slide 53: problem instance
    // slide 56: optimal flow
    // vertex number  = vertex number in slide - 1
    // edge numbering = starting from leftmost vertex: NE, SE;
    //                  return to leftmost vertex: SE
    //                  return to topmost vertex: S, NE
    val flow = computeFlow (
      supply   = Vector(4, 0, 0, -4),
      edges    = Vector((0,1), (1,3), (0,2), (1,2), (2,3)),
      cost     = Vector(  2  ,   3  ,   2  ,   1  ,   1  ),
      capacity = Vector(  4  ,   3  ,   2  ,   2  ,   5  )
    )
    assert(flow == Seq(2, 0, 2, 2, 4))
  }
}
