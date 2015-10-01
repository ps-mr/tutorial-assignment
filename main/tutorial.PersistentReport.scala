package tutorial

import minCostFlow.Graph._
import collection.mutable.MutableList

class PersistentReport(override val graph: PersistentGraph, flow: Flow)
extends Report(graph, flow) {
  import graph._

  class TutorBuffer {
    val remains = Array(groupSizeOfTutor: _*)
    val assigned = Array.fill(tutors.size)(MutableList.empty[Int])
    val isAssigned = collection.mutable.Set.empty[Int] // collection of assigned students

    // should be called before calling "add to slot"
    // to ensure all preassigned gets a place
    def addPreassigned(student: Int, slot: Int, tutor: Int): Unit = {
      // sanity check: optimizer-produced slot agrees with preassigned slot
      assert(slotOfStudent(student) == Some(slot))
      assert(slotOfTutor(tutor) == Some(slot))
      assign(student, tutor)
    }

    // has no effect on pre-assigned students
    def addToSlot(student: Int, slot: Int): Unit =
      if (! isAssigned(student)) {
        // there should be some vacancy at the time slot
        val Some(tutor) = tutorsOfSlot(slot).find(i => remains(i) > 0)
        assign(student, tutor) // also updates isAssigned
      }

    def result: IndexedSeq[Seq[Int]] = assigned

    // has no effect on already assigned students
    private[this]
    def assign(student: Int, tutor: Int): Unit =
      if (! isAssigned(student)) {
        // check that vacancy exists
        assert(remains(tutor) > 0)
        remains(tutor) -= 1
        assigned(tutor) += student
        isAssigned += student
      }
  }

  // make sure pre-assigned students stay with their tutors
  // @return mapping tutor-id to sequences of the tutor's students
  override def computeStudentsOfTutor: IndexedSeq[Seq[Int]] = {
    val buffer = new TutorBuffer
    for ( (student, (slot, tutor)) <- preassigned )
      buffer.addPreassigned(student, slot, tutor)
    for ( (maybeSlot, student) <- slotOfStudent.zipWithIndex ; slot <- maybeSlot )
      // has no effect on preassigned students
      buffer.addToSlot(student, slot)
    buffer.result
  }

  // return sequence of students newly assigned in this assignment
  def assignedStudents: Seq[data.Student] =
    for {
      (studentData, student) <- userData.validStudents.zipWithIndex
      if ! studentData.isAssigned(tutorData) // student wasn't assigned before
      group <- formatAssignedGroup(student) // student is assigned now
    }
    yield studentData.copy(assignedGroup = Some(group))

  def formatAssignedGroup(student: Int): Option[String] =
    for {
      slot <- slotOfStudent(student)
      tutor = tutorOfStudent(student)
    }
    yield tutorData.formatSlotTutor(slot, tutor)
}
