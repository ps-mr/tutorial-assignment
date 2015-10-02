import java.io.File
import spray.json._
import DefaultJsonProtocol._

package object config {
  // read a json file from the secret directory
  def secretJson[T: JsonReader](filename: String): T = {
    val secretDir = "scheduler-secret"
    val file = new java.io.File(getClass.getResource("").getPath, s"../../../../$secretDir/$filename")
    io.Source.fromFile(file).mkString.parseJson.convertTo[T]
  }

  // address a file in the data directory
  def datafile(relativePath: String): File =
    new File(getClass.getResource("").getPath, s"../../../../data/$relativePath")

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

  // this comes first because other fields depends on it
  val credential = secretJson[Map[String, String]]("credential.json")

  // other fields sorted in alphabetic order
  val List(assigned_group, assigned_at) = secretJson[List[String]]("assigned_at.json")

  val apiKey        = ("api_key", credential("api_key"))
  val dataKey       = "data"
  val dump          = credential("dump")
  val email         = "email"
  val error         = "error"
  val id            = "id"
  val name          = "name"
  val setUserField  = credential("set_user_field")
  val setUserFields = credential("set_user_fields")
  val success       = "success"
  val timeslots     = secretJson[Vector[String]]("timeslots.json")
  val truth         = "true"
  val userid        = "userid"
  val userfield     = "userfield"
  val username      = "username"
  val users         = "users"
  val value         = "value"
}
