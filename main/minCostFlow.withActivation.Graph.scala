/** PROBLEM Min-cost-flow-with-activation
  *
  * INSTANCE
  * Set of edges to be activated, and
  * Graph where
  * - every edge has a capacity, a binary activation variable, and a cost
  * - every vertex has a supply
  *
  * OBJECTIVE FUNCTION
  * The sum of flow * cost for every edge,
  *
  * CONSTRAINTS
  * Other than the usual constraints of flow <= capacity * activation
  * and outgoing-flow - incoming-flow == supply, we let the user define
  * additional constraints on the activation variables.
  */

package minCostFlow.withActivation

import minCostFlow.Graph._
import optimus.algebra.Constraint
import optimus.optimization._

trait Graph {
  val supply   : Supply
  val edges    : Edges
  val cost     : Cost
  val capacity : Capacity

  val activationEdges    : IndexedSeq[Edge]

  lazy val edgeIndices = Range(0, edges.length)

  /** @return (problem, flow, activation)
    */
  def createIntegerProgram(): (MIProblem, IndexedSeq[MPIntVar], IndexedSeq[MPIntVar]) =
    createIntegerProgramWithAdditionalConstraints((_, _, _) => Seq.empty)

  def createIntegerProgramWithAdditionalConstraints(
    additionalConstraints: (MIProblem, IndexedSeq[MPIntVar], IndexedSeq[MPIntVar]) => Seq[Constraint]
  ):
      (MIProblem, IndexedSeq[MPIntVar], IndexedSeq[MPIntVar]) =
  {
    import optimus.algebra._

    implicit val problem = MIProblem(SolverLib.gurobi)

    val flow = edgeIndices.map {
      i => MPIntVar("flow" + i, Range(0, capacity(i) + 1))
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

    val constraints = capacityConstraints ++ supplyConstraints ++ additionalConstraints(problem, flow, activation)

    minimize { (for (i <- edgeIndices) yield flow(i) * cost(i)).fold[Expression](0)(_ + _) }
    subjectTo(constraints: _*)

    (problem, flow, activation)
  }
}
