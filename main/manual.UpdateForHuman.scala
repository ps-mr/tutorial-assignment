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
    val valid   = students.validStudents

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

    val tutorUploads =
      tutors.usernames.zipWithIndex.flatMap {
        case (username, tutor) =>
          val tutorAsStudent = staff.users.validStudents(tutor)
          val assignedStudents = report.studentsOfTutor(tutor)
          if (assignedStudents.nonEmpty) {
            // customize what to put in tutor's group assignment field
            val ag4human = report.formatAssignedGroupForTutorOf(assignedStudents.head).get

            if (tutorAsStudent.assignedGroupForHuman != Some(ag4human))
              Some(tutorAsStudent.copy(assignedGroupForHuman = Some(ag4human)))
            else
              None
          }
          else
            None
      }


    if (validUploads.nonEmpty || tutorUploads.nonEmpty) {
      checkWithUser(s"""The human-readable "${config.assignedGroupForHuman}" changed. Upload changes?""")

      if (validUploads.nonEmpty)
        reportUploadTime(s"${validUploads.size} valid students") {
          setUserFields(validUploads.map(_.toFieldForHuman))
        }

      // upload tutor's assignment
      if (tutorUploads.nonEmpty)
        reportUploadTime(s"${tutorUploads.size} tutors") {
          setUserFields(tutorUploads.map(_.toFieldForHuman))
        }


      // save report to file
      reportTime(s"Writing ${config.assignmentFile}") {
        val assignmentFileContent =
          report.forHuman(Rooms.current, tutors, students.validStudents)

        util.writeFile(config.baseFile(config.assignmentFile), assignmentFileContent)
      }
    }
  }

  private[this]
  def reportUploadTime(whom: String)(what: => Unit): Unit =
    reportTime(s"""Uploading "${config.assignedGroupForHuman}" of $whom""")(what)
}
