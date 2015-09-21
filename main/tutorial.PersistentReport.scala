package tutorial

import minCostFlow.Graph._
import collection.mutable.MutableList

class PersistentReport(override val graph: PersistentGraph, flow: Flow)
extends Report(graph, flow) {
  import graph._

  class TutorBuffer {
    val remains = Array(groupSizeOfTutor: _*)
    val assigned = Array.fill(tutors.size)(MutableList.empty[Int])

    def addPreassigned(student: Int, tutor: Int): Unit = ???

    def addToSlot(student: Int, slot: Int): Unit = ???

    def result: IndexedSeq[Seq[Int]] = ???
  }

  // make sure pre-assigned students stay with their tutors
  override def computeStudentsOfTutor: IndexedSeq[Seq[Int]] = {
    ???
  }
}
