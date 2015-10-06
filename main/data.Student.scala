package data

import spray.json._
import config.{truth, checked}

object Student extends DefaultJsonProtocol {
  /** output case class */
  case class Field(userid: Int, userfield: String, value: String)

  /** configurable field names */
  object Field {
    import config._
    implicit object FieldFormat extends RootJsonWriter[Field] {
      def write(f: Field): JsValue =
        JsObject(Map[String, JsValue](
          userid    -> JsNumber(f.userid),
          userfield -> JsString(f.userfield),
          value     -> JsString(f.value)))
    }
  }

  implicit object StudentFormat extends RootJsonFormat[Student] {
    def write(s: Student): JsValue =
      JsObject(Map[String, JsValue](
        (  config.id             -> JsNumber(s.id)) +:
          (config.username       -> s.username.toJson) +:
          (config.name           -> s.name.toJson) +:
          (config.email          -> s.email.toJson) +:
          (config.assigned_group -> s.assignedGroup.toJson) +:
          (config.assignedGroupForHuman -> s.assignedGroupForHuman.toJson) +:
          (config.user_fields -> s.userFields.toJson) +:
          config.slotNames.zip(s.availability.map {
            case true  => JsString(truth)
            case false => JsNull
          }): _*))

    // don't care about preference,
    // consider first choice as important as third choice
    def read(value: JsValue): Student =
      value match {
        case JsObject(field) =>
          import config._

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
              slot   <- slotNames
              result <- field.get(slot)
            }
            yield result match {
              case JsString(s) => checked(s)
              case JsNull      => false
              case _           => deserializationError("tutorial choice must be a string or NULL")
            },

            assignedGroupForHuman =
              field.get(assignedGroupForHuman).flatMap(_.convertTo[Option[String]]),

            userFields =
              field.get(user_fields).map(_.convertTo[Map[String, Option[String]]])
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
  assignedGroup: Option[String] = None,
  assignedGroupForHuman: Option[String] = None,
  userFields: Option[Map[String, Option[String]]] = None
) {
  import Student._

  def toFields: Seq[Student.Field] =
    Seq(Field(userid = id, userfield = config.assigned_group, value = assignedGroup.get))

  def toFieldForHuman: Student.Field =
    Field(userid = id, userfield = config.assignedGroupForHuman, value = assignedGroupForHuman.get)

  // if assignedGroup == None, then slot == tutor == None
  //
  // if assignedGroup == Some(code) and code has format slot-tutor_username
  // (e. g., "tuesday_08-alec"), then slot and tutor are defined.
  val (slot, tutor): (Option[String], Option[String]) =
    assignedGroup match {
      case Some(code) =>
        val (s, _t) = code.span(_ != '-')
        if (_t.nonEmpty)
          (Some(s), Some(_t.tail))
        else
          (None, None) // wrong format
      case None =>
        (None, None)
    }

  // return Some(i) only if student is assigned to a slot where he's available
  def slotIndex(tutors: Tutors): Option[Int] =
    for {
      s <- slot
      i = tutors.slotNames.indexOf(s)
      if i >= 0 && availability(i)
    }
    yield i

  // return Some(i) only if student is assigned to an existent tutor
  def tutorIndex(tutors: Tutors): Option[Int] =
    for {
      t <- tutor
      i = tutors.usernames.indexOf(t)
      if i >= 0
    }
    yield i

  def isAssigned(tutors: Tutors): Boolean =
    slotIndex(tutors).nonEmpty && tutorIndex(tutors).nonEmpty

  def formatIdUsername: String = f"  $id%3d  $username"
}
