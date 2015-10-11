import java.io.File
import spray.json._

package object config {
  // fields obtained by reading config files
  import DefaultJsonProtocol._

  // the magic file containing the names of other files
  // make them lazy vals to do IO as little as possible
  lazy val config = baseJson[Map[String, String]](configFile)

  lazy val rooms = baseJson[Map[String, Vector[String]]](config("rooms"))

  lazy val credential = baseJson[Map[String, String]](config("credential"))

  lazy val List(assigned_group, assigned_at) = baseJson[List[String]](config("assigned_at"))

  lazy val parameters = baseJson[Map[String, Vector[Int]]](config("parameters"))

  // other fields in alphabetic order, are defs due to interdependency
  def assignedGroupForHuman = config("assigned_group_for_human")
  def assignmentFile = config("assignment")
  def apiKey        = ("api_key", credential("api_key"))
  def configFile    = "config.json"
  def dataKey       = "data"
  def dotJson       = ".json"
  def dump          = credential("dump")
  def email         = "email"
  def error         = "error"
  def groups        = "groups"
  def id            = "id"
  def listFields    = credential("list_fields")
  def listStaff     = credential("list_staff")
  def marginal_rank = "marginal_rank"
  def marginal_cost = "marginal_cost"
  def name          = "name"
  def roomPrefix    = rooms("affixes").head
  def roomSuffix    = rooms("affixes").last
  def setUserField  = credential("set_user_field")
  def setUserFields = credential("set_user_fields")
  def showUser      = credential("show_user")
  def slotNames     = rooms(timeslots)
  def success       = "success"
  def timeslots     = "timeslots"
  def truth         = "true"
  def tutors        = "tutors"
  def tutorsFile    = config("tutors")
  def unassigned_penalty = "unassigned_penalty"
  def unassignedForHuman = "Keins"
  def userid        = "userid"
  def userfield     = "userfield"
  def user_fields   = "user_fields"
  def username      = "username"
  def users         = "users"
  def usersURL      = credential("usersURL")
  def value         = "value"


  // helper functions

  // read a json file from the data directory
  def dataJson[T: JsonReader](filename: String): T =
    fromJsonFile[T](dataFile(filename))

  def fromJsonFile[T: JsonReader](file: File): T =
    io.Source.fromFile(file).mkString.parseJson.convertTo[T]

  def baseJson[T: JsonReader](path: String): T =
    fromJsonFile[T](baseFile(path))

  // address a file in the base directory
  def baseFile(relativePath: String): File =
    new File(getClass.getResource("").getPath, s"../../../../$relativePath")

  // address a file in the data directory
  def dataFile(relativePath: String): File =
    baseFile(s"data/$relativePath")

  // whether a checkbox is checked or not
  def checked(s: String): Boolean = s.nonEmpty

  def appendApiKey(url: String): String =
    url + s"?${apiKey._1}=${apiKey._2}"

  // extract first integer occuring in a string
  def getFirstInt(string: String): Int = {
    val start = string.indexWhere(_.isDigit)
    val end   = {
      val endIndex = string.indexWhere(! _.isDigit, start)
      if (endIndex < 0) string.length else endIndex
    }
    string.substring(start, end).toInt
  }
}
