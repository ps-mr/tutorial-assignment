package data

import spray.json._
import java.io.File
import config.{datafile, getFirstInt}

object Users {
  def fromJson(jsonCode: String): Users = new Users({
    def error() = deserializationError(s"""expect {"${config.users}":[ students... ]}""")
    jsonCode.parseJson match {
      case JsObject(fields) => fields(config.users) match {
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
        prefs.toList.sorted
      }

    // produce students with id in the thousands
    val users = new Users(preferences.zipWithIndex.map {
      case (pref, i) =>
        Student (
          id = i * 1000,
          username = s"username$i",
          name = s"student$i",
          email = s"no-reply$i@example.com",
          availability = Range(0, Rooms.dummy.slotNames.size).map(pref.contains)
        )
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

  val dumpedStudents: Seq[Student] = _students

  // no sense scheduling students without any available slot
  val validStudents : Seq[Student] = _students.filter {
    s => s.availability.nonEmpty && s.availability.max
  }

  // subclasses should override this if the mapping to group names is different
  protected[this]
  def getGroupName(): IndexedSeq[String] = config.timeslots

  val groupName   : IndexedSeq[String]   = getGroupName()
  val groupRank   : Map[String, Int]     = groupName.zipWithIndex.toMap

  val studentId   : IndexedSeq[Int]      = validStudents.map(_.id)(breakOut)
  val studentRank : Map[Int, Int]        = studentId.zipWithIndex.toMap

  val preferences : IndexedSeq[Seq[Int]] =
    validStudents.map(_.availability.zipWithIndex.withFilter(_._1).map(_._2))(breakOut)
  def groups      : Int                  = groupName.length
  def numberOfValidStudents : Int        = preferences.length
}
