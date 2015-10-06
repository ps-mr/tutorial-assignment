package minCostFlow

import org.scalatest._

class IntegerProgramSpec extends SolverSpec(IntegerProgram)

abstract class SolverSpec(solver: Solver) extends FlatSpec {
  def solverName: String = solver.getClass.getSimpleName.replaceFirst("\\$$", "")

  import solver.computeFlow

  // cargo code by ichoran
  // http://www.scala-lang.org/old/node/8022
  def time[A](doCompute: => A): A = {
    val t0 = System.nanoTime
    val answer = doCompute
    info(f"Elapsed: ${(System.nanoTime - t0) * 1e-9}%.3f sec\n")
    answer
  }

  solverName should "handle the empty instance" in {
    val flow = time(computeFlow(Instance.empty))
    assert(flow == Instance.emptyFlow)
  }

  it should "handle x -> y" in {
    val flow = time(computeFlow(Instance.oneEdge))
    assert(flow == Instance.oneEdgeFlow)
  }

  it should "handle x -> y -> z" in {
    val flow = time(computeFlow(Instance.twoEdgePath))
    assert(flow == Instance.twoEdgePathFlow)
  }

  it should "handle a triangle with the long path cheaper" in {
    val flow = time(computeFlow(Instance.triangle))
    assert(flow == Instance.triangleFlow)
  }

  it should "handle the example in Fernandez's slides for Uni Heidelberg" in {
    val flow = time(computeFlow(Instance.fernandez))
    assert(flow == Instance.fernandezFlow)
  }
}
