package minCostFlow

import collection._

object Relaxation {

  type Vertex       = Int
  type Edge         = Int
  type Supply       = IndexedSeq[Int]
  type Edges        = IndexedSeq[(Vertex, Vertex)]
  type Cost         = IndexedSeq[Int]
  type Capacity     = IndexedSeq[Int]
  type Flow         = IndexedSeq[Int]
  type Potential    = IndexedSeq[Int]
  type Excess       = IndexedSeq[Int]
  type Cut          = Seq[Int]
  // type OutNeighbors = IndexedSeq[Seq[Vertex]] // XXX check if needed
  // type EdgeIndex    = IndexedSeq[IndexedSeq[Int]]

  def computeFlow(supply: Supply, edges: Edges, cost: Cost, capacity: Capacity): Flow = {

    verifyMinCostInstance(supply, edges, cost, capacity)

    // basic graph properties
    val n                           = supply.length // number of vertices
    val m                           = edges.length  // number of edges

    // XXX: verify whether these are needed.
    // val outNeighbors : OutNeighbors = computeOutNeighbors(n, edges)
    // val edgeIndex    : EdgeIndex    = computeEdgeIndex(n, edges)

    var startingVertex : Vertex = -1

    // initialize attributes
    var flow        : Flow      = Vector.fill(m)(0)
    var potential   : Potential = Vector.fill(n)(0)
    var excess      : Excess = computeExcess(supply, edges, flow)
    var reducedCost : Cost   = computeReducedCost(cost, edges, potential)

    // while the network contains a node s with e(s) > 0
    while ({ startingVertex = excess.indexWhere(_ > 0) ; startingVertex >= 0 }) {
      var excessTree = ExcessTree (
        edges          = edges,
        excess         = excess,
        flow           = flow,
        potential      = potential,
        capacity       = capacity,
        reducedCost    = reducedCost,
        startingVertex = startingVertex
      )

      // priority potential adjustment
      if (excessTree.totalExcess > excessTree.potentialResidual) {
          ??? // adjust potential, recompute flow, potential, excess, reduced cost
      }
      else {

        while({
          excessTree.expansion match {
            case Some(newExcessTree) =>
              excessTree = newExcessTree
              excessTree.totalExcess <= excessTree.potentialResidual

            case None =>
              false
          }
        }) ()

        if (excessTree.totalExcess > excessTree.potentialResidual) {
          ??? // adjust potential, recompute flow, potential, excess, reducedCost
        }
        else {
          ??? // adjust flow, recompute excess
        }
      }
    }

    ???
  }

  object ExcessTree {
    def apply(
      edges          : Edges,
      excess         : Excess,
      flow           : Flow,
      potential      : Potential,
      capacity       : Capacity,
      reducedCost    : Cost,

      startingVertex : Vertex
    ): ExcessTree = {
      val treeNodes = Set(startingVertex)
      val cut = computeCut(treeNodes, edges)
      new ExcessTree(
        edges             = edges,
        excess            = excess,
        flow              = flow,
        potential         = potential,
        capacity = capacity,
        reducedCost = reducedCost,
        treeNodes         = treeNodes,
        previousEdge      = immutable.IntMap.empty,
        totalExcess       = excess(startingVertex),
        cut               = cut,
        potentialResidual = computePotentialResidual(cut, reducedCost, flow, capacity)
      )
    }
  }

  case class ExcessTree(
    edges             : Edges,
    excess            : Excess,
    flow              : Flow,
    potential         : Potential,
    capacity          : Capacity,
    reducedCost       : Cost,

    treeNodes         : Set[Vertex],
    previousEdge      : Map[Vertex, Int /* index of edge */],
    totalExcess       : Int,
    cut               : Cut,
    potentialResidual : Int
  ) {

    // postcondition: if result is None, then the candidate edge's target
    //                has negative excess (or, it has a deficit).
    def expansion: Option[ExcessTree] = {
      // find an arc in the cut in the residual network with reduced cost 0
      val candidate = cut.find {
        case e =>
          val residualCapacity = capacity(e) - flow(e)
          assert(residualCapacity >= 0)
          residualCapacity > 0 && reducedCost(e) == 0
      }

      // Since the potential residual is positive, at least
      // one arc in the cut has 0 reduced cost.
      val e = candidate.get

      if (excess(getTarget(e, edges)) >= 0)
          Some(addEdge(e))
      else
          None
    }

    // precondition: getTarget(e, edges) has nonnegative excess and is outside treeNodes
    def addEdge(e: Int): ExcessTree = {
      val target          = getTarget(e, edges)
      val newTreeNodes    = treeNodes + target
      val newCut          = computeCut(newTreeNodes, edges)
      copy (
        treeNodes         = newTreeNodes,
        previousEdge      = previousEdge.updated(target, e),
        totalExcess       = totalExcess + excess(target),
        cut               = newCut,
        potentialResidual = computePotentialResidual(newCut, reducedCost, flow, capacity)
      )
    }

    // TODO
    // 2. finalize attribute list
    // 3. check sanity
  }

  def computePotentialResidual(cut: Cut, reducedCost: Cost, flow: Flow, capacity: Capacity): Int = {
    val residualCapacity = for {
      e <- cut
      if reducedCost(e) == 0
    } yield capacity(e) - flow(e)

    residualCapacity.sum
  }


  def computeCut(vertexSet: Set[Vertex], edges: Edges): Seq[Int] =
    for {
      ((source, target), e) <- edges.zipWithIndex
      if vertexSet.contains(source) && ! vertexSet.contains(target)
    }
    yield e

  /* XXX check if necessary
  def computeEdgeIndex(n: Int, edges: Edges): EdgeIndex = {
    val edgeIndex: mutable.IndexedSeq[mutable.IndexedSeq[Int]] =
      Array.fill(n)(Array.fill(n)(-1): mutable.IndexedSeq[Int])

    edgeIndex
  }

  def computeOutNeighbors(n: Int, edges: Edges): OutNeighbors = {
    val outNeighbors: mutable.IndexedSeq[mutable.MutableList[Vertex]] =
      Array.fill(n)(mutable.MutableList.empty[Vertex])

    for ((source, target) <- edges)
      target +=: outNeighbors(source)

    outNeighbors
  }

   */

  def computeReducedCost(cost: Cost, edges: Edges, potential: Potential): Cost =
    edges.zip(cost).map {
      case ((source, target), cost) =>
        cost - potential(source) + potential(target)
    }

  def computeExcess(supply: Supply, edges: Edges, flow: Flow): Excess = {
    val excess: mutable.IndexedSeq[Int] = Array(supply: _*)

    for ( ((source, target), flow) <- edges zip flow ) {
      excess(source) -= flow
      excess(target) += flow
    }

    excess
  }

  def getSource(edge: Edge, edges: IndexedSeq[(Vertex, Vertex)]): Vertex = edges(edge)._1
  def getTarget(edge: Edge, edges: IndexedSeq[(Vertex, Vertex)]): Vertex = edges(edge)._2

  def verifyMinCostInstance(supply: Supply, edges: Edges, cost: Cost, capacity: Capacity): Unit = {
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
