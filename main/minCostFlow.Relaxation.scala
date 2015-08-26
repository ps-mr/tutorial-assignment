package minCostFlow

import collection._
import Graph._

object Relaxation extends Solver {

  def computeFlow(graph: Graph): Flow =
    computeFlow (
      supply   = graph.supply,
      edges    = graph.edges,
      cost     = graph.cost,
      capacity = graph.capacity
    )

  def computeFlow(supply: Supply, edges: Edges, cost: Cost, capacity: Capacity): Flow = {

    verifyMinCostInstance(supply, edges, cost, capacity)

    // basic graph properties
    val n                           = supply.length // number of vertices
    val m                           = edges.length  // number of edges

    var startingVertex : Vertex = -1

    // initialize attributes
    var flow        : Flow        = Vector.fill(m)(0)
    var potential   : Potential   = Vector.fill(n)(0)
    var excess      : Excess      = computeExcess(supply, edges, flow)
    var reducedCost : ReducedCost = computeReducedCost(cost, edges, potential)

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
        val (_potential, _flow, _excess, _reducedCost) = adjustPotential(excessTree, supply, cost)
        potential = _potential; flow = _flow; excess = _excess; reducedCost = _reducedCost
      }
      else {

        while({
          val expansion = excessTree.expansion
          excessTree = expansion.get
          expansion.shouldContinue
        }) ()

        if (excessTree.totalExcess > excessTree.potentialResidual) {
          val (_potential, _flow, _excess, _reducedCost) = adjustPotential(excessTree, supply, cost)
          potential = _potential; flow = _flow; excess = _excess; reducedCost = _reducedCost
        }
        else {
          val (_flow, _excess) = adjustFlow(excessTree, supply)
          flow = _flow; excess = _excess
        }
      }
    }

    flow
  }

  def adjustPotential(old: ExcessTree, supply: Supply, cost: Cost):
      (Potential, Flow, Excess, ReducedCost) =
  {
    val flow: mutable.IndexedSeq[Int] = Array(old.flow: _*)

    for (e <- old.cut; if old.reducedCost(e) == 0)
      // send residualCapacity(e) units of flow down e, making it full
      flow(e) = old.capacity(e)

    val excess = computeExcess(supply, old.edges, flow)

    val alphaCandidates = for {
      e <- old.cut
      if old.capacity(e) > flow(e)
    } yield old.reducedCost(e)

    // alphaCandidates can be empty. if they are, the instance is infeasible.
    val alpha = alphaCandidates.min

    val potential = old.potential.zipWithIndex.map {
      case (pi, v) if old.treeNodes contains v =>
        pi + alpha

      case (pi, v) =>
        pi
    }

    val reducedCost = computeReducedCost(cost, old.edges, potential)

    (potential, flow, excess, reducedCost)
  }

  def adjustFlow(old: ExcessTree, supply: Supply): (Flow, Excess) = {
    val path = pathToRoot(old)

    val pathResidualCapacity: List[Int] = path.map(e => old.capacity(e) - old.flow(e))(breakOut)

    val delta = (old.excess(old.firstVertex) :: - old.excess(old.lastVertex) :: pathResidualCapacity).min

    assert(delta > 0)

    val flow: mutable.IndexedSeq[Int] = Array(old.flow: _*)
    for (e <- path) flow(e) += delta

    val excess = computeExcess(supply, old.edges, flow)

    (flow, excess)
  }

  def pathToRoot(excessTree: ExcessTree): Seq[Edge] = {
    val path = mutable.MutableList.empty[Edge]
    var v    = excessTree.lastVertex

    while ({
      excessTree.previousEdge.get(v) match {
        case Some(e) =>
          v = getSource(e, excessTree.edges)
          e +=: path
          true

        case None =>
          false
      }
    }) ()

    path
  }

  object ExcessTree {
    def apply(
      edges          : Edges,
      excess         : Excess,
      flow           : Flow,
      potential      : Potential,
      capacity       : Capacity,
      reducedCost    : ReducedCost,

      startingVertex : Vertex
    ): ExcessTree = {
      val treeNodes = Set(startingVertex)
      val cut = computeCut(treeNodes, edges)
      new ExcessTree(
        edges             = edges,
        excess            = excess,
        flow              = flow,
        potential         = potential,
        capacity          = capacity,
        reducedCost       = reducedCost,

        treeNodes         = treeNodes,
        previousEdge      = immutable.IntMap.empty,
        totalExcess       = excess(startingVertex),
        cut               = cut,
        potentialResidual = computePotentialResidual(cut, reducedCost, flow, capacity),
        firstVertex       = startingVertex,
        lastVertex        = startingVertex
      )
    }
  }

  sealed trait TreeExpansion { def shouldContinue: Boolean ; def get: ExcessTree }
  case class Success(get: ExcessTree) extends TreeExpansion {
    def shouldContinue = get.totalExcess <= get.potentialResidual
  }
  case class Failure(get: ExcessTree) extends TreeExpansion {
    def shouldContinue = false
  }

  case class ExcessTree(
    edges             : Edges,
    excess            : Excess,
    flow              : Flow,
    potential         : Potential,
    capacity          : Capacity,
    reducedCost       : ReducedCost,

    treeNodes         : Set[Vertex],
    previousEdge      : Map[Vertex, Edge],
    totalExcess       : Int,
    cut               : Cut,
    potentialResidual : Int,
    firstVertex       : Vertex,
    lastVertex        : Vertex
  ) {

    // postcondition: if result is None, then the candidate edge's target
    //                has negative excess (or, it has a deficit).
    def expansion: TreeExpansion = {

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
          Success(addNodeAndEdge(e))
      else
          Failure(addEdge(e))
    }

    // precondition: getTarget(e, edges) has nonnegative excess and is outside treeNodes
    def addNodeAndEdge(e: Int): ExcessTree = {
      val target          = getTarget(e, edges)
      val newTreeNodes    = treeNodes + target
      val newCut          = computeCut(newTreeNodes, edges)
      copy (
        treeNodes         = newTreeNodes,
        previousEdge      = previousEdge.updated(target, e),
        totalExcess       = totalExcess + excess(target),
        cut               = newCut,
        potentialResidual = computePotentialResidual(newCut, reducedCost, flow, capacity),
        lastVertex        = target
      )
    }

    // precondition: getTarget(e, edges) has negative excess
    def addEdge(e: Int): ExcessTree = {
      val target = getTarget(e, edges)
      copy (
        previousEdge      = previousEdge.updated(target, e),
        lastVertex        = target
      )
    }
  }

  def computePotentialResidual(cut: Cut, reducedCost: ReducedCost, flow: Flow, capacity: Capacity): Int = {
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

  def computeReducedCost(cost: Cost, edges: Edges, potential: Potential): ReducedCost =
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

  def verifyMinCostInstance(supply: Supply, edges: Edges, cost: Cost, capacity: Capacity): Unit = {
    val n = supply.length // number of vertices
    val m = edges.length  // number of edges

    // edges are between existing vertices
    if (m > 0)
      for ((i, j) <- edges) if (! (0 <= i & i < n & 0 <= j & j < n))
        sys.error(s"bad edge: $i $j")

    // costs are attached to existing edges
    assert(m == cost.length)

    // capacities are attached to existing edges
    assert(m == capacity.length)
  }
}
