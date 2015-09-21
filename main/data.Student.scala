package data

import spray.json._
import remote.Forum.secretJson
import remote.Forum.credential.{truth, checked}

object Student extends DefaultJsonProtocol {
  /** output case class */
  case class Field(userid: Int, userfield: String, value: String)

  /** configurable field names */
  object Field {
    val id = "id"
    val name = "name"
    val username = "username"
    val email = "email"
    lazy val timeslots = secretJson[Vector[String]]("timeslots.json")
    lazy val List(assigned_group, assigned_at) = secretJson[List[String]]("assigned_at.json")

    val (userid, userfield, value) = {
      import remote.Forum.credential.{userid => i, userfield => f, value => v}
      (i, f, v)
    }
  }

  implicit object FieldFormat extends RootJsonWriter[Field] {
    import Field._
    def write(f: Field): JsValue =
      JsObject(Map[String, JsValue](
        userid    -> JsNumber(f.userid),
        userfield -> JsString(f.userfield),
        value     -> JsString(f.value)))
  }

  implicit object StudentFormat extends RootJsonFormat[Student] {
    def write(s: Student): JsValue =
      s.toFields.toVector.map(FieldFormat.write).toJson

    // don't care about preference,
    // consider first choice as important as third choice
    def read(value: JsValue): Student =
      value match {
        case JsObject(field) =>
          import Field._

          Student(
            id = field(id).convertTo[Int],

            name = field(name) match {
              case JsString(s) => s
              case JsNull => "Anonymous"
              case _ => deserializationError("student name must be a string or NULL")
            },

            username = field(username).convertTo[String],

            email = field(email).convertTo[String],

            assignedGroup = field(assigned_group).convertTo[Option[String]],

            availability = for {
              slot   <- timeslots
              result <- field.get(slot)
            }
            yield result match {
              case JsString(s) => checked(s)
              case JsNull      => false
              case _           => deserializationError("tutorial choice must be a string or NULL")
            }
          )

        case _ =>
          deserializationError("student expected")
      }
  }
}

// activerecord students in Scala
case class Student(
  id: Int,
  username: String,
  name: String,
  email: String,
  availability: Seq[Boolean],
  assignedGroup: Option[String] = None
) {
  import Student._

  def toFields: collection.GenTraversable[Student.Field] =
    assignedGroup.map {
      case v =>
        Field(userid = id, userfield = Field.assigned_group, value = v)
    }

  // if assignedGroup == None, then slot == tutor == None
  //
  // if assignedGroup == Some(code) and code has format slot-tutor_username
  // (e. g., "tuesday_08-alec"), then slot and tutor are defined.
  val (slot, tutor): (Option[String], Option[String]) =
    assignedGroup match {
      case Some(code) =>
        val (s, _t) = code.span(_ != '-')
        (Some(s), Some(_t.tail))
      case None =>
        (None, None)
    }

  // precondition: this.assignedGroup != None
  def tutorIndex(tutors: Tutors): Int = {
    val index = tutors.usernames.indexOf(tutor.get)
    assert(index >= 0)
    index
  }
}
