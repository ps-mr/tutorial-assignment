package manual

/** Download tutor data from forum and compare it with cached tutor data.
  * Presumably cached tutor data were used in previous scheduling.
  * This manual process performs integrity check to ensure that
  * previously scheduled students are not rescheduled willy-nilly
  * when some tutors make unilateral changes to their username,
  * availability, or thei existence of their accounts.
  *
  * Run it with sensible integrity check:
  *
  * manual.Process.execute(manual.DownloadTutors(remote.Forum.getUsers))
  */

import data._

object DownloadTutors extends (Users => DownloadTutors) {
  def apply(students: Users): DownloadTutors = new DownloadTutors(students)
}

class DownloadTutors(students: Users) extends Process[Tutors] {
  def run(): Tutors = {
    val cached  = reportTime(s"Reading ${config.tutorsFile}") { Staff.lastSaved() }
    val current = reportTime("Downloading tutors from forum") { remote.Forum.getStaff() }

    val oldTutors = cached.toTutors
    val newTutors = current.toTutors

    if (oldTutors == newTutors) {
      println(s"The cache ${config.tutorsFile} is up to date.")
      newTutors
    }
    else if (oldTutors.slotNames != newTutors.slotNames)
      fail("""|Time slots changed.
              |Did you forget to update config.json in the new semester?""".stripMargin)
    else {
      import Staff.{UsernameChange, AvailabilityChange}

      val (usernameChanges, availabilityChanges, insertions, deletions) = cached.conflict(current)

      if (usernameChanges.nonEmpty) {
        val changes = usernameChanges.map {
          case UsernameChange(prev, next) =>
            f"- Tutor ${prev.id}%2d renamed ${prev.username} ==> ${next.username}"
        }
        fail("Tutor username change detected.\n" + changes.mkString("\n"))
      }
      else if (availabilityChanges.nonEmpty || insertions.nonEmpty || deletions.nonEmpty) {
        // go through the possible changes with the user, check if they are okay.
        println("\nTutors changed.")

        // check whether the availability change conflict with existing assignments.
        // if it doesn't conflict, query user to save conflicted tutors in scheduler secret,
        // and remind user to commit the secret directory.
        // if it conflicts with existing assignment, then ask user to fix it.
        val constraints =
          for {
            s     <- students.validStudents
            slot  <- s.slotIndex(oldTutors)
            tutor <- s.tutorIndex(oldTutors)
          }
          yield (s, slot, cached.getTutor(tutor))

        val invalidConstraints =
          for {
            (student, slot, tutor) <- constraints
            // we've checked that time slots are the same in current and cached tutors.
            if ! (current.contains(tutor) && current.getUpdated(tutor).get.availability(slot))
          }
          yield student

        if (invalidConstraints.nonEmpty)
          fail(
            "The following group assignments are invalidated:\n" +
            invalidConstraints.map({
              case s =>
                f"  student id=${s.id}%4d user=${s.username}%s assigned ${s.assignedGroup}%s"
            }).mkString("\n"))


        if (deletions.nonEmpty) {
          println("The following tutors are deleted:")
          println(deletions.map(_.format).mkString("\n"))
          checkWithUser("Is it okay to delete them?")
        }

        if (insertions.nonEmpty) {
          println("\nThe following tutors are added:")
          println(insertions.map(_.format).mkString("\n"))
          checkWithUser("Is it okay to add them?")
        }

        // everything's fine, save tutor cache and proceed
        // print tutors with changed availabilities for information
        if (availabilityChanges.nonEmpty) {
          println("\nThe following tutors changed their availability without causing conflict:")
          println(availabilityChanges.map("  " + _.next.username).mkString("\n"))
        }

        reportTime(s"\nSaving ${config.tutorsFile}") { current.saveToFile() }
        newTutors
      }
      else
        internalError("Tutors' availability changed without being detected by Staff.conflict.")
    }
  }
}
