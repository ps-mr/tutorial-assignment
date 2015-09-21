/** Tutorial scheduling graph with consideration for
  * pre-assigned students.
  *
  * A student is considered pre-assigned if it's assigned
  * to a time slot it's available and an existing tutor.
  */

package tutorial

import optimus.optimization._
import optimus.algebra._
import minCostFlow.Graph._

class PersistentGraph(
  val userData      : data.Users,
  val roomData      : data.Rooms,
  val tutorData     : data.Tutors,
  marginalRank      : Seq[Int],
  marginalCost      : Seq[Int],
  unassignedPenalty : Int
) extends Graph(userData, roomData, tutorData, marginalRank, marginalCost, unassignedPenalty)
{
  // take pre-assigned students into consideration
  override
  def createIntegerProgramWithAdditionalConstraints(
    additionalConstraints:
        (MIProblem, IndexedSeq[MPIntVar], IndexedSeq[MPIntVar]) => Seq[Constraint]
  ): (MIProblem, IndexedSeq[MPIntVar], IndexedSeq[MPIntVar]) =
    super.createIntegerProgramWithAdditionalConstraints {
      case (problem, flow, activation) =>
        implicit val _problem = problem

        // preassigned students: studentIndex => (slotIndex, tutorIndex)
        val preassigned: Map[Int, (Int, Int)] =
          for {
            _ <- Map.empty[Int, (Int, Int)]
            (student, studentIndex) <- userData.validStudents.zipWithIndex
            slotIndex               <- student.slotIndex
            tutorIndex              <- student.tutorIndex(tutorData)
          }
          yield (studentIndex, (slotIndex, tutorIndex))

        // mapping students to slots
        val studentToSlot: Map[Int, Int] = preassigned.mapValues(_._1)

        // mapping tutors to slots
        val tutorToSlot: Map[Int, Int] = preassigned.map(p => (p._2._2, p._2._1))

        // MAYBE should verify here that no tutor is assigned two slots?

        val studentConstraints =
          for {
            e <- edgeIndices
            studentIndex  = students.indexOf(getSource(e, edges))
            expectedSlot <- studentToSlot.get(studentIndex)
            actualSlot    = slots.indexOf(getTarget(e, edges))
            if expectedSlot == actualSlot
          }
          yield flow(e) := 1

        val tutorConstraints =
          for {
            (e, i) <- activationEdges.zipWithIndex
            tutorIndex    = tutors.indexOf(getTarget(e, edges))
            expectedSlot <- tutorToSlot.get(tutorIndex)
            actualSlot    = slots.indexOf(getSource(e, edges))
            if expectedSlot == actualSlot
          }
          yield activation(i) >= 1 // forces tutor-timeslot assignment

        studentConstraints ++ tutorConstraints
    }
}
