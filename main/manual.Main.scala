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
          println(s"\nWARNING: The following ${report.unassignedTutors.size} tutors are unassigned:")
          println(
            report.unassignedTutors.
              map(staff.users.validStudents).
              map(_.formatIdUsername).mkString("\n"))
        }

        if (report.unassignedStudents.nonEmpty) {
          println(s"\nWARNING: The following ${report.unassignedStudents.size} students are unassigned:")
          println(
            report.unassignedStudents.
              map(students.validStudents).
              map(_.formatIdUsername).mkString("\n"))
        }
      }
    }
    yield
      Process.chooseByUser("\nUpload assignment?\nIf no, I will display assignment and halt.") {
        // ask whether to display new assignment to upload
        if (fieldsToUpload.nonEmpty) {
          Process.chooseByUser("\nDisplay the JSON to be uploaded?") {
            println(fieldsToUpload.map("  " + _.toJson).mkString("\n"))
            println()
          } { () }
        }

        // upload assignment, abort if disagreed
        Process.execute {
          for {
            uploadAssignment <- if (fieldsToUpload.nonEmpty) {
              new UploadAssignment(fieldsToUpload)
            }
            else {
              println("No change in student assignment.")
              Process.Return(())
            }

            // update the human-readable version of assigned_group
            updateForHuman <- new UpdateForHuman(students, staff, tutors, report)
          }
          yield ()
        }
      } {
        println()
        println(report.forHuman(data.Rooms.current, tutors, students.validStudents))
      }
  }

}
