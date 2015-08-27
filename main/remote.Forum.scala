package remote

import data._
import scalaj.http._
import spray.json._

object Forum {
  object credential {
    val map: Map[String, String] = {
      // secret, not to be shared
      val credentialFile = new java.io.File(getClass.getResource("").getPath,
        "../../../../data/credential.json")
      import DefaultJsonProtocol._
      scala.io.Source.fromFile(credentialFile).mkString.
        parseJson.convertTo[Map[String, String]]
    }

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

  def setUserField(userid: Int, key: String, value: String): Unit = {
    val response: HttpResponse[String] =
      Http(credential.setUserField).
        postForm(Seq(
          credential.apiKey,
          (credential.userid    , userid.toString),
          (credential.userfield , key),
          (credential.value     , value)
        )).asString

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
