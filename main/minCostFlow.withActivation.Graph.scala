/** PROBLEM Min-cost-flow-with-activation
  *
  * INSTANCE
  * Graph where
  * - every edge has a capacity an activation-cost and a flow-cost
  * - every vertex has a supply
  *
  * OBJECTIVE FUNCTION
  * The sum of flow * flow-cost for every edge,
  * plus the activation-cost if the flow is nonzero
  *
  * A min-cost-flow-with-activation instance has all the method
  * of a min-cost-flow instance, but the former is not a subtype
  * of the latter, because their objective functions are
  * different.
  */

package minCostFlow.withActivation

import minCostFlow.Graph._
import optimus.optimization._

object Graph {
  import optimus.algebra.{Expression, Const}

  sealed trait Activeness {
    def toDouble     : Double
    def toExpression : Expression
  }

  case class ActiveVariable(variable: MPIntVar) extends Activeness {
    def toDouble     : Double     = variable.value.get
    def toExpression : Expression = variable
  }

  case object AlwaysActive extends Activeness {
    def toDouble     : Double     = 1.0
    def toExpression : Expression = toDouble
  }
}

trait Graph {
  val supply         : Supply
  val edges          : Edges
  val activationCost : Cost
  val flowCost       : Cost
  val capacity       : Capacity

  /** @return (problem, flow, activation)
    */
  def createIntegerProgram(): (MIProblem, IndexedSeq[MPIntVar], IndexedSeq[Graph.Activeness]) = {
    import optimus.algebra._
    import Graph._

    implicit val problem = MIProblem(SolverLib.oJalgo)

    val edgeIndices = Range(0, edges.length)

    val flow = edgeIndices.map {
      case i => MPIntVar("flow" + i, Range(0, capacity(i) + 1))
    }

    val activation = edgeIndices.map {
      case i if activationCost(i) == 0 => AlwaysActive
      case i if activationCost(i) != 0 => ActiveVariable(MPIntVar("activation" + i, 0 to 1))
    }

    val capacityConstraints = for (i <- edgeIndices) yield flow(i) <= capacity(i) * activation(i).toExpression

    val supplyConstraints = for {
      v <- 0 until supply.length
      outgoingFlow = (for { i <- edgeIndices ; if v == getSource(i, edges) } yield flow(i)).fold[Expression](0)(_ + _)
      incomingFlow = (for { i <- edgeIndices ; if v == getTarget(i, edges) } yield flow(i)).fold[Expression](0)(_ + _)
    }
    yield outgoingFlow - incomingFlow := supply(v)

    val constraints = capacityConstraints ++ supplyConstraints

    minimize {
      (  for (i <- edgeIndices)
         yield flow(i) * flowCost(i) + activation(i).toExpression * activationCost(i)
      ).fold[Expression](0)(_ + _)
    }
    subjectTo(constraints: _*)

    (problem, flow, activation)
  }
}
