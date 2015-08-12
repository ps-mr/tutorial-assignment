package minCostFlow

object Relaxation {

  def minCostFlow(
    supply   : IndexedSeq[Int],
    edges    : IndexedSeq[(Int, Int)],
    cost     : IndexedSeq[Int],
    capacity : IndexedSeq[Int]
  ): IndexedSeq[Int] = {
    type Vertex = Int
    type Edge   = Int
    def getSource(edge: Edge): Vertex = edges(edge)._1
    def getTarget(edge: Edge): Vertex = edges(edge)._2

    val n = supply.length // number of vertices
    val m = edges.length  // number of edges

    var flow      : Vector[Int] = Vector.fill(m)(0)
    var potential : Vector[Int] = Vector.fill(n)(0)

    ???
  }

  def verifyMinCostInstance(
    supply   : IndexedSeq[Int],
    edges    : IndexedSeq[(Int, Int)],
    cost     : IndexedSeq[Int],
    capacity : IndexedSeq[Int]
  ): Unit = {
    val n = supply.length // number of vertices
    val m = edges.length  // number of edges

    // edges are between existing vertices
    if (m > 0)
      assert(
        edges.map({
          case (i, j) => 0 <= i & i < n & 0 <= j & j < n
        }).min
      )

    // costs are attached to existing edges
    assert(m == cost.length)

    // capacities are attached to existing edges
    assert(m == capacity.length)
  }
}
