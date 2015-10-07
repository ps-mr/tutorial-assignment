package manual

class UploadAssignment(fields: Seq[data.Student.Field]) extends Process[Unit] {
  def run(): Unit = {
    Process.checkWithUser("Okay to upload?")

    reportTime("Uploading") {
      remote.Forum.setUserFields(fields)
    }
  }
}
