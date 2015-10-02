package util

// JsValue operations: dynamic type testing without tears (for the user)
import spray.json._

package object jsOps {
  implicit class JsOps(js: JsValue) {
    def asObject: Map[String, JsValue] = tryObject.get

    def tryObject: Option[Map[String, JsValue]] =
      js match {
        case JsObject(o) => Some(o)
        case _ => None
      }

    def asArray: Vector[JsValue] = tryArray.get

    def tryArray: Option[Vector[JsValue]] =
      js match {
        case JsArray(a) => Some(a)
        case _ => None
      }

    def asString: String = tryString.get

    def tryString: Option[String] =
      js match {
        case JsString(s) => Some(s)
        case _ => None
      }

    def asNumber: BigDecimal = tryNumber.get

    def tryNumber: Option[BigDecimal] =
      js match {
        case JsNumber(n) => Some(n)
        case _ => None
      }

    def asBoolean: Boolean = tryBoolean.get

    def tryBoolean: Option[Boolean] =
      js match {
        case JsBoolean(b) => Some(b)
        case _ => None
      }

    def asNull: Null = tryNull.get

    def tryNull: Option[Null] =
      js match {
        case JsNull => Some(null)
        case _ => None
      }
  }
}
