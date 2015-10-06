package manual

import tutorial.PersistentReport
import data._
import Process._
import remote.Forum._

class UpdateForHuman(
  students: Users,
  staff: Staff,
  tutors: Tutors,
  report: PersistentReport
) extends Process[Unit]
{

  def run(): Unit = {
    val invalid = students.invalidStudents
    val valid   = students.validStudents

    val invalidUploads =
      for {
        student <- invalid
        if student.assignedGroupForHuman != Some(config.unassignedForHuman)
      }
      yield student.copy(assignedGroupForHuman = Some(config.unassignedForHuman))

    val validUploads =
      for {
        (student, i) <- valid.zipWithIndex
        ag4human = report.formatAssignedGroupForHuman(i) match {
          case None => config.unassignedForHuman
          case Some(s) => s
        }
        if student.assignedGroupForHuman != Some(ag4human)
      }
      yield student.copy(assignedGroupForHuman = Some(ag4human))

    if (invalidUploads.nonEmpty || validUploads.nonEmpty) {
      checkWithUser(s"""The human-readable "${config.assignedGroupForHuman}" changed. Upload changes?""")

      if (invalidUploads.nonEmpty)
        reportUploadTime(s"${invalidUploads.size} invalid students") {
          setUserFields(invalidUploads.map(_.toFieldForHuman))
        }

      if (validUploads.nonEmpty)
        reportUploadTime(s"${validUploads.size} valid students") {
          setUserFields(validUploads.map(_.toFieldForHuman))
        }
    }

    val tutorUploads =
      tutors.usernames.zipWithIndex.flatMap {
        case (username, tutor) =>
          val tutorAsStudent = staff.users.validStudents(tutor)
          val assignedStudents = report.studentsOfTutor(tutor)
          if (assignedStudents.nonEmpty) {
            val ag4human = report.formatAssignedGroupForHuman(assignedStudents.head).get
            Some((username, ag4human, tutorAsStudent))
          }
          else
            None
      }

      // uploading user field values is too buggy.
      // I probably destroyed many tutors' Matrikelnummer and Studiengang.
      // Let's just print their slots and email them the report.
    if (tutorUploads.nonEmpty) {
      println("Tutor-room assignments:")

      tutorUploads.foreach {
        case (username, value, tutor) =>
          println("\n" + value + " (forum username)")
      }
    }
  }

  private[this]
  def reportUploadTime(whom: String)(what: => Unit): Unit =
    reportTime(s"""Uploading "${config.assignedGroupForHuman}" of $whom""")(what)
}
