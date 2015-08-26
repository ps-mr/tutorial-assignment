package minCostFlow

import org.scalatest._

class RelaxationSpec extends SolverSpec(Relaxation)


abstract class SolverSpec(solver: Solver) extends FlatSpec {
  def solverName: String = solver.getClass.getSimpleName.replaceFirst("\\$$", "")

  import solver.computeFlow

  solverName should "handle the empty instance" in {
    val flow = computeFlow(Instance.empty)
    assert(flow == Instance.emptyFlow)
  }

  it should "handle x -> y" in {
    val flow = computeFlow(Instance.oneEdge)
    assert(flow == Instance.oneEdgeFlow)
  }

  it should "handle x -> y -> z" in {
    val flow = computeFlow(Instance.twoEdgePath)
    assert(flow == Instance.twoEdgePathFlow)
  }

  it should "handle a triangle with the long path cheaper" in {
    val flow = computeFlow(Instance.triangle)
    assert(flow == Instance.triangleFlow)
  }

  it should "handle the example in Fernandez's slides for Uni Heidelberg" in {
    val flow = computeFlow(Instance.fernandez)
    assert(flow == Instance.fernandezFlow)
  }
}
