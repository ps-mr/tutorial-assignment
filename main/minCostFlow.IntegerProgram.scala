package minCostFlow

import optimus.optimization._
import Graph._

object IntegerProgram extends Solver {
  def computeFlow(graph: Graph): Flow = {

    implicit val (problem, flowVars) = graph.createIntegerProgram()

    // tolerance of rounding inaccuracy
    // is such that every float deviating for this much changes
    // their sum by less than 0.5
    val epsilon: Double = 0.25 / flowVars.size

    start()
    val objective = objectiveValue
    val flow = flowVars.map {
      case v =>
        val fpn = v.value.get
        val int = Math.round(fpn).toInt
        if (Math.abs(fpn - int) > epsilon)
          sys error s"rounding flow = $fpn with epsilon = $epsilon"
        int
    }
    release()

    flow
  }

  /** execute mixed integer programming example from Optimus */
  def runExample() {
    implicit val problem = MIProblem(SolverLib.oJalgo)
    val x = MPFloatVar("x", 0, 40)
    val y = MPIntVar("y", 0 to 1000)
    val z = MPIntVar("z", 0 until 18)
    val t = MPFloatVar("t", 2, 3)

    maximize(x + 2*y + 3*z + t)
    subjectTo (
      -1*x + y + z + 10*t <= 20,
      x - 3.0*y + z <= 30,
      y - 3.5*t := 0
    )

    start()
    println("objective: " + objectiveValue)
    println("x = " + x.value + "y = " + y.value + "z = " + z.value + "t = " + t.value)

    release()
  }
}
