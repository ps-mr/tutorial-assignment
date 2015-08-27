/** PROBLEM Min-cost-flow-with-activation
  *
  * INSTANCE
  * Matrix A, vector b, set of edges to be activated,
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
  *   A alpha = b,
  *
  * where alpha is the activation vector,
  *           A is the coefficient matrix,
  *           b is the expected result.
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

  val activationEdges  : IndexedSeq[Edge]
  val activationMatrix : Graph.Matrix
  val activationResult : IndexedSeq[Int]

  /** @return (problem, flow, activation)
    */
  def createIntegerProgram(): (MIProblem, IndexedSeq[MPIntVar], IndexedSeq[MPIntVar]) = {
    import optimus.algebra._
    import Graph._

    implicit val problem = MIProblem(SolverLib.oJalgo)

    val edgeIndices = Range(0, edges.length)

    val flow       = edgeIndices    .map { i => MPIntVar("flow" + i, Range(0, capacity(i) + 1)) }
    val activation = activationEdges.map { i => MPIntVar("activation" + i, 0 to 1) }

    val capacityConstraints = for (i <- edgeIndices) yield flow(i) <= capacity(i) * activation(i)

    val supplyConstraints = for {
      v <- 0 until supply.length
      outgoingFlow = (for { i <- edgeIndices ; if v == getSource(i, edges) } yield flow(i)).fold[Expression](0)(_ + _)
      incomingFlow = (for { i <- edgeIndices ; if v == getTarget(i, edges) } yield flow(i)).fold[Expression](0)(_ + _)
    }
    yield outgoingFlow - incomingFlow := supply(v)

    assert(activationMatrix.rows == activationResult.length)
    val activationConstraints = ((activationMatrix * activation), activationResult).zipped.map {
      case (lhs, rhs) => lhs := rhs
    }

    val constraints = capacityConstraints ++ supplyConstraints ++ activationConstraints

    minimize { (for (i <- edgeIndices) yield flow(i) * cost(i)).fold[Expression](0)(_ + _) }
    subjectTo(constraints: _*)

    (problem, flow, activation)
  }
}
