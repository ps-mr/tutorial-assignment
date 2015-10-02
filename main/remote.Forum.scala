package remote

import data._
import scalaj.http._
import spray.json._
import DefaultJsonProtocol._

object Forum {
  def secretJson[T: JsonReader](filename: String): T = {
    val secretDir = "scheduler-secret"
    val file = new java.io.File(getClass.getResource("").getPath, s"../../../../$secretDir/$filename")
    io.Source.fromFile(file).mkString.parseJson.convertTo[T]
  }

  object credential {
    val map = secretJson[Map[String, String]]("credential.json")

    val apiKey       = ("api_key", map("api_key"))
    val dump         = map("dump")
    val setUserField = map("set_user_field")
    val setUserFields= map("set_user_fields")

    val dataKey      = "data"
    val userid       = "userid"
    val userfield    = "userfield"
    val value        = "value"

    val success      = "success"
    val truth        = "true"
    val error        = "error"

    // whether a checkbox is checked or not
    def checked(s: String): Boolean = s.nonEmpty

    def dumpWithApiKey: String = {
      val (key, value) = apiKey
      s"$dump?$key=$value"
    }
  }

  def dump: String = {
    val response: HttpResponse[String] =
      Http(credential.dumpWithApiKey).asString

    if (response.isError)
      sys error response.toString
    else
      response.body
  }

  implicit class HttpOps(http: HttpRequest) {
    // must be called last, because HttpRequest.postXYZ set method to POST.
    def doPut: HttpRequest = http.method("PUT")

    def asJson: HttpRequest = http.header("Content-type", "application/json")
  }

  def appendApiKey(url: String): String =
    url + s"?${credential.apiKey._1}=${credential.apiKey._2}"

  def putJson(url: String, json: JsValue): HttpRequest =
    Http(appendApiKey(url)).postData(json.compactPrint).asJson.doPut

  def setUserFields(fields: Seq[data.Student.Field]): Unit = {
    val json = Map(credential.dataKey -> fields.map(_.toJson)).toJson
    val response = putJson(credential.setUserFields, json).asString
    if (response.isError)
      sys error response.toString
  }

  def setUserField(userid: Int, key: String, value: String): Unit = {
    val response: HttpResponse[String] =
      Http(credential.setUserField).postForm(
        Seq(
          credential.apiKey,
          (credential.userid    , userid.toString),
          (credential.userfield , key),
          (credential.value     , value)
        )
      ).doPut.asString

    if (response.isError)
      sys error response.toString
    else {
      import DefaultJsonProtocol._
      val map = response.body.parseJson.convertTo[JsValue].asJsObject
      map.fields.get(credential.success) match {
        case Some(JsBoolean(true)) => ()
        case _                     => sys error map.prettyPrint
      }
    }
  }

  def getUsers(): Users = Users.fromJson(dump)
}
