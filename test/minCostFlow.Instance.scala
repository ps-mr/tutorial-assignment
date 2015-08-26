package minCostFlow

import org.scalatest._

import Graph._

object Instance {
  val empty: Graph = Simple (
    supply   = Vector.empty,
    edges    = Vector.empty,
    cost     = Vector.empty,
    capacity = Vector.empty
  )

  val emptyFlow: Flow = Vector.empty

  val oneEdge: Graph = Simple (
    supply   = Vector(10, -10),
    edges    = Vector((0, 1)),
    cost     = Vector(10),
    capacity = Vector(10)
  )

  val oneEdgeFlow: Flow = Vector(10)

  val twoEdgePath: Graph = Simple (
    supply   = Vector(10, 0, -10),
    edges    = Vector((0, 1), (1, 2)),
    cost     = Vector(10, 10),
    capacity = Vector(10, 10)
  )

  val twoEdgePathFlow: Flow = Vector(10, 10)

  val triangle: Graph = Simple (
    supply   = Vector(1, 0, -1),
    edges    = Vector((0, 2), (0, 1), (1, 2)),
    cost     = Vector(20, 5, 5),
    capacity = Vector(1, 1, 1)
  )

  val triangleFlow: Flow = Vector(0, 1, 1)

  // http://www.iwr.uni-heidelberg.de/groups/comopt/teaching/ws10/effAlgI/ueb/fernandez_mincostflow.pdf
  // slide 53: problem instance
  // slide 56: optimal flow
  // vertex number  = vertex number in slide - 1
  // edge numbering = starting from leftmost vertex: NE, SE;
  //                  return to leftmost vertex: SE
  //                  return to topmost vertex: S, NE
  val fernandez: Graph = Simple (
    supply   = Vector(4, 0, 0, -4),
    edges    = Vector((0,1), (1,3), (0,2), (1,2), (2,3)),
    cost     = Vector(  2  ,   3  ,   2  ,   1  ,   1  ),
    capacity = Vector(  4  ,   3  ,   2  ,   2  ,   5  )
  )

  val fernandezFlow: Flow = Vector(2, 0, 2, 2, 4)
}
