import java.io.File
import spray.json._

package object config {
  // fields obtained by reading config files
  import DefaultJsonProtocol._

  // the magic file containing the names of other files
  val config = baseJson[Map[String, String]]("config.json")

  val rooms = baseJson[Map[String, Vector[String]]](config("rooms"))

  val credential = baseJson[Map[String, String]](config("credential"))

  val List(assigned_group, assigned_at) = baseJson[List[String]](config("assigned_at"))

  // other fields in alphabetic order, are defs due to interdependency
  def apiKey        = ("api_key", credential("api_key"))
  def dataKey       = "data"
  def dump          = credential("dump")
  def email         = "email"
  def error         = "error"
  def id            = "id"
  def name          = "name"
  def setUserField  = credential("set_user_field")
  def setUserFields = credential("set_user_fields")
  def slotNames     = rooms(timeslots)
  def success       = "success"
  def timeslots     = "timeslots"
  def truth         = "true"
  def userid        = "userid"
  def userfield     = "userfield"
  def username      = "username"
  def users         = "users"
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

  // dump url with api key appended
  def dumpWithApiKey: String = appendApiKey(dump)

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
