package manual

object Main extends App {

  Process.execute {
    for {
      // load config files
      _ <- LoadConfig
      (marginalRank, marginalCost, unassignedPenalty) <- LoadParameters
      rooms <- LoadRooms

      // download students and tutors from forum
      students <- DumpStudents
      (staff, tutors) <- new DownloadTutors(students)

      graph <- new BuildPersistentGraph(students, rooms, tutors, marginalRank, marginalCost, unassignedPenalty)

      report <- new ComputePersistentReport(graph)

      newlyAssigned = report.assignedStudents
      preassigned   = students.validStudents.size - newlyAssigned.size

      // print newly assigned students for humans
      fieldsToUpload = {
        println()
        println(f"$preassigned%3d preassigned students.")
        println(f"${newlyAssigned.size}%3d newly assigned students.")

        // TODO: singularize to Student.toField, handle display separately
        newlyAssigned.flatMap(_.toFields)
      }

      warnAboutUnassignedStudentsAndTutors = {
        if (report.unassignedTutors.nonEmpty) {
          println("\nWARNING: The following tutors are unassigned:")
          println(
            report.unassignedTutors.
              map(staff.users.validStudents).
              map(_.formatIdUsername).mkString("\n"))
        }

        if (report.unassignedStudents.nonEmpty) {
          println("\nWARNING: The following students are unassigned:")
          println(
            report.unassignedStudents.
              map(students.validStudents).
              map(_.formatIdUsername).mkString("\n"))
        }
      }

      // ask whether to display new assignment to upload
      displayFullReport = Process.chooseByUser("\nDisplay the JSON to be uploaded?") {
        println(fieldsToUpload.map("  " + _.toJson).mkString("\n"))
      } { () }

      // upload assignment
      uploadAssignment <- new UploadAssignment(fieldsToUpload)

      // TODO: after adding display field, update that in a separate setUserFields.
    }
    yield ()
  }

}
