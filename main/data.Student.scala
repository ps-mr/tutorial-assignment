package data

import spray.json._
import remote.Forum.secretJson
import remote.Forum.credential.truth

object Student extends DefaultJsonProtocol {
  /** configurable field names */
  object Field {
    val id = "id"
    val name = "name"
    val email = "email"
    lazy val timeslots = secretJson[Vector[String]]("timeslots.json")
    lazy val List(assigned_group, assigned_at) = secretJson[List[String]]("assigned_at.json")
  }


  implicit object StudentFormat extends RootJsonFormat[Student] {
    def write(s: Student): JsValue =
      JsObject(Map[String, JsValue](
        (Field.id -> JsNumber(s.id)) +:
          Field.timeslots.zip(s.availability.map(
            avail => if (avail) JsString(truth) else JsNull
          )): _*))

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

            email = field(email).convertTo[String],

            availability = for {
              slot   <- timeslots
              result <- field.get(slot)
            }
            yield result match {
              case JsString(s) => s == truth
              case JsNull      => false
              case _           => deserializationError("tutorial choice must be a string or NULL")
            }
          )

        case _ =>
          deserializationError("student expected")
      }
  }
}

case class Student(
  id: Int,
  name: String,
  email: String,
  availability: Seq[Boolean]
)
