package manual

import tutorial.{PersistentGraph, PersistentReport}

class ComputePersistentReport(graph: PersistentGraph) extends Process[PersistentReport] {
  def run(): PersistentReport = {
    // should report flow computation time
    println("Computing flow...")
    Console.flush()
    val start = now()
    val report = graph.computeReport()
    val end = now()
    val elapsed = (end - start) / 1000.0
    println(f"\nFlow computed. ($elapsed%.2f s)")
    report
  }
}
