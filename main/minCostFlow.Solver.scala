package minCostFlow

trait Solver {
  def computeFlow(graph: Graph): Graph.Flow
}
