package data

import spray.json._
import java.io.File
import config.{dataFile, getFirstInt}
import util.jsOps._

// tutor's time availability downloaded from Discourse

object Staff {
  // use Discourse API to obtain json of tutors
  def fromRemote(jsonCode: String): Staff = {
    import DefaultJsonProtocol._
    // download field names and ids from the forum
    val fieldIds = remote.Forum.fieldIds
    val tutors: Seq[Student] =
      for {
        entry <- jsonCode.parseJson.asArray
        table = entry.asObject
        id = table(config.id).asNumber.toInt
        username = table(config.username).asString

        // download the user's fields
        user = remote.Forum.showUser(username).parseJson.asObject

        // make sure it's a tutor
        // unfortunately, must download individual user data to see
        // whether a staff is a tutor.
        groups = user(config.groups).convertTo[Seq[Map[String, JsValue]]]
        if groups.find(_(config.name) == config.tutors.toJson) != None

        name = user(config.name).asString
        fields = user(config.user_fields).asObject
        availability = config.slotNames.map {
          case slot =>
            fields(fieldIds(slot).toString).convertTo[Option[String]] match {
              case None => false
              case Some(answer) => config.checked(answer)
            }
        }
      }
      yield Student(
        id            = id,
        username      = username,
        name          = name,
        email         = "", // some tutors don't have emails
        availability  = availability,
        assignedGroup = None
      )

    Staff(new Users(tutors))
  }

  implicit object StaffFormat extends RootJsonFormat[Staff] {
    import DefaultJsonProtocol._

    // only save tutors with availability
    def write(s: Staff): JsValue =
      s.users.validStudents.toJson

    def read(value: JsValue): Staff =
      Staff(new Users(value.convertTo[Seq[Student]]))
  }
}


case class Staff(users: Users) {
  def toTutors: Tutors =
    Tutors.fromStudents(users.validStudents)

  // save tutor availability info in file
  // TODO: consistency checking and warn-on-change manual process
  def saveToFile(): Unit = {
    import java.nio.file.{Paths, Files}
    import java.nio.charset.StandardCharsets
    val path = config.baseFile(config.tutorsFile)
    val code = this.toJson.prettyPrint
    Files.write(Paths.get(path.toURI), code.getBytes(StandardCharsets.UTF_8))
  }
}
