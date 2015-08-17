package data

import spray.json._

object Users {
  /** configurable field names */
  object Field {
    val users = "users"
  }

  def apply(jsonCode: String): Users = new Users (
    jsonCode.parseJson.asJsObject.fields.toList match {
      case List((Field.users, JsArray(elements))) =>
        elements.map(_.convertTo[Student])

      case _ =>
        deserializationError(s"""expect {"${Field.users}":[ students... ]}""")
    }
  )
}


class Users(_students: Seq[Student]) {
  import collection.breakOut

  val studentsWithChoices : Seq[Student] = _students.filter(_.choices.nonEmpty)
  val groupNameSet        : Set[String]  = studentsWithChoices.flatMap(_.choices)(breakOut)

  val groupName   : IndexedSeq[String]   = groupNameSet.toVector
  val groupRank   : Map[String, Int]     = groupName.zipWithIndex.toMap

  val studentId   : IndexedSeq[Int]      = studentsWithChoices.map(_.id)(breakOut)
  val studentRank : Map[Int, Int]        = studentId.zipWithIndex.toMap

  val preferences : IndexedSeq[Seq[Int]] = studentsWithChoices.map(_.choices.map(groupRank))(breakOut)
  def groups      : Int                  = groupName.length
  def students    : Int                  = preferences.length
}
