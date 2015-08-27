package data

import spray.json._
import java.io.File
import Rooms.{datafile, getFirstInt} // extract?

object Users {
  /** configurable field names */
  object Field {
    val users = "users"
  }

  def fromJson(jsonCode: String): Users = new Users({
    def error() = deserializationError(s"""expect {"${Field.users}":[ students... ]}""")
    jsonCode.parseJson match {
      case JsObject(fields) => fields(Field.users) match {
        case JsArray(elements) =>
          elements.map(_.convertTo[Student])

        case _ => error()
      }

      case _ => error()
    }
  })

  lazy val year2014: Users = fromFile(datafile("group-prefs-2014.txt"))

  def fromFile(file: File): Users = {
    val line     = io.Source.fromFile(file).getLines
    val students = getFirstInt(line.next)
    val tutors   = getFirstInt(line.next)

    val preferences: IndexedSeq[Seq[Int]] =
      Vector.fill(students) {
        val prefs = (line.next().split("\\s+").map(_.toInt): Seq[Int])
        prefs
      }

    // produce students with id in the thousands
    val users = new Users(preferences.zipWithIndex.map {
      case (pref, i) =>
        Student(i * 1000, pref.map(Rooms.dummy.slotNames))
    }) {
      protected[this]
      override def getGroupName(): IndexedSeq[String] = Rooms.dummy.slotNames
    }

    assert(users.preferences == preferences)

    users
  }
}


class Users(_students: Seq[Student]) {
  import collection.breakOut

  val studentsWithChoices : Seq[Student] = _students.filter(_.choices.nonEmpty)

  // subclasses should override this if the mapping to group names is known
  protected[this]
  def getGroupName(): IndexedSeq[String] = studentsWithChoices.flatMap(_.choices)(breakOut).toVector

  val groupName   : IndexedSeq[String]   = getGroupName()
  val groupRank   : Map[String, Int]     = groupName.zipWithIndex.toMap

  val studentId   : IndexedSeq[Int]      = studentsWithChoices.map(_.id)(breakOut)
  val studentRank : Map[Int, Int]        = studentId.zipWithIndex.toMap

  val preferences : IndexedSeq[Seq[Int]] = studentsWithChoices.map(_.choices.map(groupRank))(breakOut)
  def groups      : Int                  = groupName.length
  def students    : Int                  = preferences.length
}
