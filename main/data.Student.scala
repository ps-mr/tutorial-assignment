package data

import spray.json._

object Student extends DefaultJsonProtocol {
  /** configurable field names */
  object Field {
    val id      = "id"
    val choices = (1 to 5).map("choice" + _)
  }


  implicit object StudentFormat extends RootJsonFormat[Student] {
    def write(s: Student): JsValue =
      JsObject(Map[String, JsValue](
        (Field.id -> JsNumber(s.id)) +:
          Field.choices.zip(s.choices.map(JsString.apply)): _*))

    // don't care about preference,
    // consider first choice as important as third choice
    def read(value: JsValue): Student =
      value match {
        case JsObject(field) =>
          import Field.{id, choices}
          Student(
            id      = field(id).convertTo[Int],
            choices = for {
              choice <- choices
              result <- field.get(choice)
              string <- result match {
                case JsString(s) => Some(s)
                case JsNull      => None
                case _           => deserializationError("tutorial choice must be a string or NULL")
              }
            }
            yield string)

        case _ =>
          deserializationError("student expected")
      }
  }
}

case class Student(
  id: Int,
  choices: Seq[String]
) {

  // idea: custom json protocol

  //val jsonData: Map[String, Seq[Map[String, 
}
