package tutorial

import org.scalatest._
import data._

import spray.json._
import DefaultJsonProtocol._

class PersistentGraphSpec
extends GraphSpecTrait[PersistentReport, PersistentGraph]("tutorial.PersistentGraph")
{
  def mkGraph = PersistentGraph.apply

  // lazy because this should initialize after superclass is fully
  // initialized.
  lazy val assignedStudents = report.assignedStudents

  it should "format student assignment correctly" in {
    val Seq(g1, g2, g3, g4, g5) = assignedStudents.take(5).map(_.assignedGroup.get)

    // this test is solver-dependent.
    assert(g1 == "monday_20-alec")
    assert(g2 == "monday_18-antonia")
    assert(g3 == "tuesday_08-abigail")
    assert(g4 == "monday_22-abraham")
    assert(g5 == "monday_18-antonia")
  }

  it should "export into JSON" in {
    val json = assignedStudents.take(3).flatMap(_.toFields).map(_.toJson).toJson.toString
    assert(json ==
      """[""" +
      """{"userid":0,"userfield":"assigned_group","value":"monday_20-alec"},""" +
      """{"userid":1000,"userfield":"assigned_group","value":"monday_18-antonia"},""" +
      """{"userid":2000,"userfield":"assigned_group","value":"tuesday_08-abigail"}]""")
  }

  // test whether persistence works by pre-assigning first 4 students,
  // with 2 valid assignments followed by 2 invalid assignments.
  val fst +: snd +: trd +: fth +: rest = users.dumpedStudents

  val newStudents = {
    fst.copy(assignedGroup = Some("monday_08-ashton"  )) +: // valid assignment
    snd.copy(assignedGroup = Some("monday_20-atticus" )) +: // valid assignment
    trd.copy(assignedGroup = Some("monday_08-ashton"  )) +: // unavailable slot
    fth.copy(assignedGroup = Some("monday_20-benjamin")) +: // nonexistent tutor
    rest
  }

  val newGraph =
    mkGraph(new Users(newStudents), rooms, tutors, marginalRank, marginalCost, unassignedPenalty)

  val newReport =
    newGraph.computeReport()

  val newlyAssigned = newReport.assignedStudents

  it should "persist valid pre-assignments" in {
    assert(newReport.formatAssignedGroup(0) == Some("monday_08-ashton"  ))
    assert(newReport.formatAssignedGroup(1) == Some("monday_20-atticus" ))
  }

  it should "not export students with valid pre-assignment" in {
    assert(newlyAssigned.find(_.id == fst.id) == None)
    assert(newlyAssigned.find(_.id == snd.id) == None)
  }

  it should "export students without valid pre-assignment" in {
    assert(newlyAssigned.find(_.id == trd.id) != None)
    assert(newlyAssigned.find(_.id == fth.id) != None)
  }
}
