package tutorial

import minCostFlow.Graph._

class Report(val graph: Graph, val flow: Flow) {
  import graph._

  def vertexToSlot(v: Vertex): Int = v - numberOfStudents

  /** find out which slot a student is assigned to */
  val slotOfStudent: IndexedSeq[Option[Int]] =
    students.map {
      case student =>
        val outEdges = edgeIndices.filter(i => getSource(i, edges) == student)
        val flows = outEdges.map(flow)
        val slotVertices = outEdges.map(i => getTarget(i, edges))
        assert(flows.count(_ == 0) == flows.length - 1 && flows.count(_ == 1) == 1)
        val targetVertex = slotVertices(flows.indexWhere(_ == 1))
        if (targetVertex == hell)
          None
        else
          Some(vertexToSlot(targetVertex))
    }

  val slotOfTutor: IndexedSeq[Option[Int]] =
    tutors.zipWithIndex.map {
      case (tutor, ti) =>
        val inEdges = edgeIndices.filter(i => getTarget(i, edges) == tutor)
        val flows = inEdges.map(flow)
        val slots = inEdges.map(i => vertexToSlot(getSource(i, edges)))
        assert(flows.count(_ == 0) >= flows.length - 1)
        val flowIndex = flows.indexWhere(_ != 0)
        if (flowIndex < 0)
          None
        else
          Some(slots(flowIndex))
    }

  val groupSizeOfTutor: IndexedSeq[Int] =
    tutors.zipWithIndex.map {
      case (tutor, ti) =>
          val outEdges = edgeIndices.filter(i => getSource(i, edges) == tutor)
          outEdges.map(flow).sum
    }

  val unassignedStudents: IndexedSeq[Int] =
    for {
      (slot_s, s) <- slotOfStudent.zipWithIndex
      if slot_s == None
    }
    yield s

  val unassignedTutors: IndexedSeq[Int] =
    for {
      (slot_t, t) <- slotOfTutor.zipWithIndex
      if slot_t == None
    }
    yield t

  val studentsOfSlot: IndexedSeq[Seq[Int]] =
    slots.zipWithIndex.map {
      case (_, thisSlot) =>
        for {
          (slot_s, s) <- slotOfStudent.zipWithIndex
          if slot_s == Some(thisSlot)
        }
        yield s
    }

  val tutorsOfSlot: IndexedSeq[Seq[Int]] =
    slots.zipWithIndex.map {
      case (_, thisSlot) =>
        for {
          (slot_t, t) <- slotOfTutor.zipWithIndex
          if slot_t == Some(thisSlot)
        }
        yield t
    }

  val studentsOfTutor: IndexedSeq[Seq[Int]] = computeStudentsOfTutor

  def computeStudentsOfTutor: IndexedSeq[Seq[Int]] = {
    val remainderOfSlot = Array(studentsOfSlot: _*)
    slotOfTutor.zipWithIndex.map {
      case (Some(slot), tutor) =>
        val (myStudents, otherStudents) =
          remainderOfSlot(slot).splitAt(groupSizeOfTutor(tutor))
        remainderOfSlot(slot) = otherStudents
        myStudents

      case (None, tutor) =>
        Seq.empty
    }
  }

  val tutorOfStudent: Map[Int, Int] =
    for {
      _ <- Map(0 -> 0) // type annotation really
      (students, tutor) <- studentsOfTutor.zipWithIndex
      student <- students
    }
    yield (student, tutor)

  def tutorsOfSlotForHuman(rooms: data.Rooms, tutors: data.Tutors, students: Seq[data.Student]):
      Seq[(String, Seq[(String, String, Seq[String])])] =
    tutorsOfSlot.zip(rooms.roomNames).zipWithIndex.map {
      case (tutorsAndRoomNames, slot) =>
        ( rooms.slotNames(slot),
          tutorsAndRoomNames.zipped.map {
            case (tutor, room) =>
              ( tutors.tutorNames(tutor)
              , room
              , studentsOfTutor(tutor).map(i => students(i).username).toList.sorted
              )
          }
        )
    }

  def forHuman(rooms: data.Rooms, tutors: data.Tutors, students: Seq[data.Student]): String =
    tutorsOfSlotForHuman(rooms, tutors, students).flatMap({
      case (slot, tutorRoomStudents) =>
        //s"\n$slot:" +:
        for {
          (tutor, room, students) <- tutorRoomStudents
        }
        yield {
          f"\n$slot%12s in $room%4s is $tutor" +
          s"\n  ${students.mkString(",")}"
        }
    }).mkString("\n")
}
