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

    val userid       = "userid"
    val userfield    = "userfield"
    val value        = "value"

    val success      = "success"
    val truth        = "true"
    val error        = "error"

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

  def putRequest(url: String, keyvals: Seq[(String, String)]): HttpRequest =
    Http(url).postForm(keyvals).copy(method = "PUT")

  // TODO: wait for forum rebuild, then fix this method.
  // Test with CURL to verify that it's okay to send api key as
  // a form variable.
  def setUserField(userid: Int, key: String, value: String): Unit = {
    val response: HttpResponse[String] =
      putRequest(
        credential.setUserField,
        Seq(
          credential.apiKey,
          (credential.userid    , userid.toString),
          (credential.userfield , key),
          (credential.value     , value)
        )
      ).asString

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
