package manual

class UploadAssignment(fields: Seq[data.Student.Field]) extends Process[Unit] {
  def run(): Unit = {
    println()
    Process.chooseByUser("Okay to upload?") {
      reportTime("Uploading") {
        remote.Forum.setUserFields(fields)
      }
    } { () }
  }
}
