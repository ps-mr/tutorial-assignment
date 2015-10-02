import java.io.File
import spray.json._

package object config {
  // fields obtained by reading config files

  // CAUTION: need to change this if current semester is not WS2015!!!
  // consider separate config file for file names?
  val rooms = {
    import DefaultJsonProtocol._
    secretJson[Map[String, Vector[String]]]("rooms-ws2015.json")
  }

  val credential = {
    import DefaultJsonProtocol._
    secretJson[Map[String, String]]("credential.json")
  }

  val List(assigned_group, assigned_at) = {
    import DefaultJsonProtocol._
    secretJson[List[String]]("assigned_at.json")
  }

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

  // read a json file from the secret directory
  def secretJson[T: JsonReader](filename: String): T =
    fromJsonFile(secretFile(filename))

  // read a json file from the data directory
  def dataJson[T: JsonReader](filename: String): T =
    fromJsonFile(dataFile(filename))

  def fromJsonFile[T: JsonReader](file: File): T =
    io.Source.fromFile(file).mkString.parseJson.convertTo[T]

  // address a file in the base directory
  def baseFile(relativePath: String): File =
    new File(getClass.getResource("").getPath, s"../../../../$relativePath")

  // address a file in the secret directory
  def secretFile(relativePath: String): File =
    baseFile(s"scheduler-secret/$relativePath")

  // address a file in the data directory
  def dataFile(relativePath: String): File =
    baseFile(s"data/$relativePath")

  // whether a checkbox is checked or not
  def checked(s: String): Boolean = s.nonEmpty

  def appendApiKey(url: String): String =
    url + s"?${config.apiKey._1}=${config.apiKey._2}"

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
