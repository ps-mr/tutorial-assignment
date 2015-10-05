package manual

import data._

object DownloadTutors extends Process[Tutors] {
  def run(): Tutors = {
    val cached  = reportTime("Reading cached tutors file") { Staff.lastSaved() }
    val current = reportTime("Downloading tutors from forum") { remote.Forum.getStaff() }

    val oldTutors = cached.toTutors
    val newTutors = current.toTutors

    if (oldTutors == newTutors) {
      println("Tutors' availability stayed as before.")
      newTutors
    }
    else {
      import Staff.{UsernameChange, AvailabilityChange}
      val (usernameChanges, availabilityChanges) = cached.conflict(current)
      if (usernameChanges.nonEmpty) {
        val changes = usernameChanges.map {
          case UsernameChange(prev, next) =>
            f"- Tutor ${prev.id}%2d renamed ${prev.username} ==> ${next.username}"
        }
        fail("Tutor username change detected.\n" + changes.mkString("\n"))
      }
      else if (availabilityChanges.nonEmpty) {
        // check whether the availability change conflict with existing assignments.
        // if it doesn't conflict, query user to save conflicted tutors in scheduler secret,
        // and remind user to commit the secret directory.
        // if it conflicts with existing assignment, then complain that the tutors
        // should change their ways.
        ???
      }
      else
        internalError("Tutors' availability changed without being detected by Staff.conflict.")
    }
  }
}
