/** PROBLEM Min-cost-flow-with-activation
  *
  * INSTANCE
  * Matrices E, L, vectors e, l, set of edges to be activated, and
  * Graph where
  * - every edge has a capacity, a binary activation variable, and a cost
  * - every vertex has a supply
  *
  * OBJECTIVE FUNCTION
  * The sum of flow * cost for every edge,
  *
  * CONSTRAINTS
  * Other than the usual constraints of flow <= capacity * activation
  * and outgoing-flow - incoming-flow == supply, we have the additional
  * constraint that
  *
  *     E alpha == e      L alpha <= l
  *
  * where alpha is the activation vector,
  * E is   equality constraint matrix, b is   equality constraint result,
  * L is inequality constraint matrix, l is inequality constraint result.
  */

package minCostFlow.withActivation

import minCostFlow.Graph._
import optimus.optimization._

object Graph {
  import optimus.algebra.{Expression, Const}

  case class Matrix(entries: IndexedSeq[IndexedSeq[Int]]) {
    val rows = entries.length
    assert(rows != 0)
    val cols = entries.head.length
    assert(cols != 0)
    assert(entries.map(_.length == cols).min)

    def * (vector: IndexedSeq[MPIntVar]): IndexedSeq[Expression] = {
      assert(cols == vector.length)
      for (row <- entries) yield
        row.zip(vector).map({ case (c, x) => x * x}).fold[Expression](0)(_ + _)
    }
  }
}

trait Graph {
  val supply   : Supply
  val edges    : Edges
  val cost     : Cost
  val capacity : Capacity

  val activationEdges    : IndexedSeq[Edge]
  val activationEqMatrix : Graph.Matrix
  val activationEqResult : IndexedSeq[Int]
  val activationLeMatrix : Graph.Matrix
  val activationLeResult : IndexedSeq[Int]

  /** @return (problem, flow, activation)
    */
  def createIntegerProgram(): (MIProblem, IndexedSeq[MPIntVar], IndexedSeq[MPIntVar]) = {
    import optimus.algebra._
    import Graph._

    implicit val problem = MIProblem(SolverLib.gurobi)

    val edgeIndices = Range(0, edges.length)

    val flow = edgeIndices.map {
      i => MPIntVar("flow" + i, Range(0, capacity(i) + 1))
      //i => MPFloatVar("flow" + i, 0, capacity(i))
    }
    val activation = activationEdges.map { i => MPIntVar("activation" + i, 0 to 1) }

    val capacityConstraints = for (i <- edgeIndices) yield {
      val activationIndex = activationEdges.indexOf(i)
      if (activationIndex >= 0)
        flow(i) <= capacity(i) * activation(activationIndex)
      else
        flow(i) <= capacity(i)
    }

    val supplyConstraints = for {
      v <- 0 until supply.length
      outgoingFlow = (for { i <- edgeIndices ; if v == getSource(i, edges) } yield flow(i)).fold[Expression](0)(_ + _)
      incomingFlow = (for { i <- edgeIndices ; if v == getTarget(i, edges) } yield flow(i)).fold[Expression](0)(_ + _)
    }
    yield outgoingFlow - incomingFlow := supply(v)

    assert(activationEqMatrix.rows == activationEqResult.length)
    val activationEqConstraints = ((activationEqMatrix * activation), activationEqResult).zipped.map {
      case (lhs, rhs) => lhs := rhs
    }

    assert(activationLeMatrix.rows == activationLeResult.length)
    val activationLeConstraints = ((activationLeMatrix * activation), activationLeResult).zipped.map {
      case (lhs, rhs) => lhs <= rhs
    }

    val constraints = capacityConstraints ++ supplyConstraints ++ activationEqConstraints ++ activationLeConstraints

    minimize { (for (i <- edgeIndices) yield flow(i) * cost(i)).fold[Expression](0)(_ + _) }
    subjectTo(constraints: _*)

    (problem, flow, activation)
  }
}
