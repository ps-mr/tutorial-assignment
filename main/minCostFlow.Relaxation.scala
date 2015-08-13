package minCostFlow

import collection._

object Relaxation {

  type Vertex       = Int
  type Edge         = Int
  type Supply       = IndexedSeq[Int]
  type Edges        = IndexedSeq[(Vertex, Vertex)]
  type Cost         = IndexedSeq[Int]
  type ReducedCost  = Cost
  type Capacity     = IndexedSeq[Int]
  type Flow         = IndexedSeq[Int]
  type Potential    = IndexedSeq[Int]
  type Excess       = IndexedSeq[Int]
  type Cut          = Seq[Int]

  // test run
  def main(args: Array[String]): Unit = {
    // empty instance
    println("empty vector == " + computeFlow(
      supply   = Vector.empty,
      edges    = Vector.empty,
      cost     = Vector.empty,
      capacity = Vector.empty
    ))

    /*
    // simple instance
    println("10 == " + computeFlow(
      supply   = Vector(10, -10),
      edges    = Vector((0, 1)),
      cost     = Vector(10),
      capacity = Vector(10)
    ))
     */

    println("(10, 10) == " + computeFlow(
      supply   = Vector(10, 0, -10),
      edges    = Vector((0, 1), (1, 2)),
      cost     = Vector(10, 10),
      capacity = Vector(10, 10)
    ))
  }

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

      //DEBUG
      println()
      println("making singleton tree")
      println(s"node = ${excessTree.treeNodes}")
      println(s"cut  = ${excessTree.cut}")
      println(s"e/r  = (${excessTree.totalExcess}, ${excessTree.potentialResidual})")
      println(s"pi   = ${excessTree.potential}")
      println(s"x    = ${excessTree.flow}")
      println(s"e    = ${excessTree.excess}")
      println(s"c_pi = ${excessTree.reducedCost}")

      println()


      // priority potential adjustment
      if (excessTree.totalExcess > excessTree.potentialResidual) {
        val (_potential, _flow, _excess, _reducedCost) = adjustPotential(excessTree, supply, cost)
        potential = _potential; flow = _flow; excess = _excess; reducedCost = _reducedCost
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

        //DEBUG
        println()
        println("fully grown tree")
        println(s"node = ${excessTree.treeNodes}")
        println(s"cut  = ${excessTree.cut}")
        println(s"e/r  = (${excessTree.totalExcess}, ${excessTree.potentialResidual})")
        println(s"pi   = ${excessTree.potential}")
        println(s"x    = ${excessTree.flow}")
        println(s"e    = ${excessTree.excess}")
        println(s"c_pi = ${excessTree.reducedCost}")
        println()


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

    // alphaCandidates can't be empty
    val alpha = alphaCandidates.min

    val potential = old.potential.zipWithIndex.map {
      case (pi, v) if old.treeNodes contains v =>
        pi + alpha

      case (pi, v) =>
        pi
    }

    val reducedCost = computeReducedCost(cost, old.edges, potential)


    //DEBUG
    println()
    println(s"adjusting potential")
    println(s"potential   = $potential")
    println(s"flow        = $flow")
    println(s"excess      = $excess")
    println(s"reducedCost = $reducedCost")
    println()
    println(s"oldential   = ${old.potential}")
    println(s"oldflow     = ${old.flow}")
    println(s"oldexcess   = ${old.excess}")
    println(s"oldRecdCost = ${old.reducedCost}")
    println()
    println(s"treeNodes   = ${old.treeNodes}")
    println(s"cut         = ${old.cut}")
    println(s"e(S)r(pi,S) = (${old.totalExcess}, ${old.potentialResidual})")
    println()

    (potential, flow, excess, reducedCost)
  }

  def adjustFlow(old: ExcessTree, supply: Supply): (Flow, Excess) = {

    //DEBUG
    println()
    println("adjusting flow")
    println(s"node = ${old.treeNodes}")
    println(s"cut  = ${old.cut}")
    println(s"e/r  = (${old.totalExcess}, ${old.potentialResidual})")
    println(s"pi   = ${old.potential}")
    println(s"x    = ${old.flow}")
    println(s"e    = ${old.excess}")
    println(s"c_pi = ${old.reducedCost}")
    println(s"s/j  = ${old.firstVertex}, ${old.lastVertex}")
    println(s"preE = ${old.previousEdge}")
    println()


    val path = pathToRoot(old)

    println(s"\ndone computing path: $path\n")//DEBUG

    val pathResidualCapacity: List[Int] = path.map(e => old.capacity(e) - old.flow(e))(breakOut)

    val delta = (old.excess(old.firstVertex) :: - old.excess(old.lastVertex) :: pathResidualCapacity).min

    //DEBUG
    println()
    println("done adjusting flow")
    println(s"delta = $delta")
    println(s"excess = ${old.excess}")
    println(s"treeNodes = ${old.treeNodes}")
    println(s"(first, last) = (${old.firstVertex}, ${old.lastVertex})")
    println(s"(e(S), r(pi, S)) = (${old.totalExcess}, ${old.potentialResidual})")
    println()

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
      println("ONCE")//DEBUG
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
        potentialResidual = computePotentialResidual(newCut, reducedCost, flow, capacity),
        firstVertex       = this.firstVertex,
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
