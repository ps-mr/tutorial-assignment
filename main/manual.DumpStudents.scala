package manual

import data._

object DumpStudents extends Process[Users] {
  def run(): Users =
    reportTime("Downloading students from forum") { remote.Forum.getUsers() }
}
