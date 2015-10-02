package data

import spray.json._
import util.jsOps._

// custom field names in discourse
object FieldNames {
  implicit object FieldNamesFormat extends RootJsonReader[FieldNames] {
    def read(js: JsValue): FieldNames = {
      val fields = js.asObject(config.user_fields).asArray

      val result = for {
        _ <- Map(0 -> "") // type annotation
        field <- fields
        obj = field.asObject
      }
      yield (obj(config.id).asNumber.toInt, obj(config.name).asString)

      FieldNames(result)
    }
  }
}

case class FieldNames(get: Map[Int, String])
