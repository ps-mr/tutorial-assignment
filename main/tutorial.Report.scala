package tutorial

import minCostFlow.Graph._

case class Report(graph: Graph, flow: Flow) {
  import graph._

  def vertexToSlot(v: Vertex): Int = v - numberOfStudents

  /** find out which slot a student is assigned to */
  val slotOfStudent: IndexedSeq[Int] =
    students.map {
      case student =>
        val outEdges = Range(0, edges.size).filter(i => getSource(i, edges) == student)
        val flows = outEdges.map(flow)
        val slots = outEdges.map(i => vertexToSlot(getTarget(i, edges)))
        assert(flows.count(_ == 0) == flows.length - 1 && flows.count(_ == 1) == 1)
        slots(flows.indexWhere(_ == 1))
    }

  val slotOfTutor: IndexedSeq[Int] =
    tutors.zipWithIndex.map {
      case (tutor, ti) =>
        val inEdges = Range(0, edges.size).filter(i => getTarget(i, edges) == tutor)
        val flows = inEdges.map(flow)
        val slots = inEdges.map(i => vertexToSlot(getSource(i, edges)))
        assert(flows.count(_ == 0) == flows.length - 1)
        slots(flows.indexWhere(_ != 0))
    }
}
