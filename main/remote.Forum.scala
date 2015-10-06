package remote

import data._
import scalaj.http._
import spray.json._
import DefaultJsonProtocol._

object Forum {
  // dump list of students using Discourse-course plugin
  def dump: String = download(config.dump)

  // download field names together with ids from the forum
  lazy val fieldNames: data.FieldNames =
    download(config.listFields).parseJson.convertTo[data.FieldNames]

  lazy val fieldIds: Map[String, Int] = fieldNames.get.map(_.swap)

  // download the user's fields from Discourse
  def showUser(username: String): String =
    download(config.showUser + username.toLowerCase + config.dotJson)

  def download(url: String): String =
      keepTrying(Http(config.appendApiKey(url))).body

  def keepTrying(http: HttpRequest): HttpResponse[String] = {
    val response: HttpResponse[String] = http.asString

    if (response.code == 429 /* too many requests */) {
      Thread.sleep(100)      /* sleep 0.1 sec     */
        this.keepTrying(http)/* try again         */
    }
    else if (response.isError)
      sys error response.toString
    else
      response
  }

  implicit class HttpOps(http: HttpRequest) {
    // must be called last, because HttpRequest.postXYZ set method to POST.
    def doPut: HttpRequest = http.method("PUT")

    def asJson: HttpRequest = http.header("Content-type", "application/json")
  }

  def putJson(url: String, json: JsValue): HttpRequest =
    Http(config.appendApiKey(url)).postData(json.compactPrint).asJson.doPut

  def setUserFields(fields: Seq[data.Student.Field]): Unit = {
    val json = Map(config.dataKey -> fields.map(_.toJson)).toJson
    expectTruth(putJson(config.setUserFields, json))
  }

  // set user field by discourse-course plugin
  // only work on unprivileged users
  def setUserField(userid: Int, key: String, value: String): Unit =
    expectTruth(
      Http(config.setUserField).postForm(
        Seq(
          config.apiKey,
          (config.userid    , userid.toString),
          (config.userfield , key),
          (config.value     , value)
        )
      ).doPut
    )

  // the hard-core way of setting a user field
  // works on everything; bugs here are dangerous.
  // =============================================
  // CAUTION:
  // NEVER set user_fields[21]=whatever.
  // This sets every other field to null.
  // =============================================
  def setUserFieldsByUsername(username: String, value: String): Unit =
    expectOK(
      Http(s"${config.usersURL}/$username.json").postForm(
        Seq(
          config.apiKey,
          config.user_fields -> value
        )
      ).doPut
    )

  def expectTruth(request: HttpRequest): Unit =
    expect(JsBoolean(true))(request)

  def expectOK(request: HttpRequest): Unit =
    expect(JsString("OK"))(request)

  def expect(expected: JsValue)(request: HttpRequest): Unit = {
    val response = keepTrying(request)
    if (response.isError)
      sys error response.toString
    else {
      import DefaultJsonProtocol._
      val map = response.body.parseJson.convertTo[JsValue].asJsObject
      map.fields.get(config.success) match {
        case Some(actual) if actual == expected => ()
        case _ => sys error map.prettyPrint
      }
    }
  }

  def getUsers(): Users = Users.fromJson(dump)
  def getStaff(): Staff = Staff.fromRemote(download(config.listStaff))

  // restore tutors' availability from cached file
  // a life-saver when Discourse decides it's okay to replace
  // user fields wholesale
  def restoreStaffAvailability(staff: Staff): Unit =
    staff.users.validStudents.foreach {
      case tutor =>
        val slots = config.slotNames.zip(tutor.availability).withFilter(_._2).map(_._1)
        val keys = slots.map(fieldIds)
        val map = Map(keys.map(id => (id.toString, config.truth)): _*).toJson
        setUserFieldsByUsername(tutor.username, map.toString)
    }
}
