package data

import java.io.File
import config.{dataFile, getFirstInt}

object Tutors {
  lazy val dummy: Tutors = fromFile(dataFile("tutors-dummy.txt"))

  def splitAtEachTab(string: String): Seq[String] = splitAtEach('\t', string)

  def splitAtEach(char: Char, string: String): Seq[String] = {
    val charLocations = string.zipWithIndex.flatMap {
      case (c, i) if c == char => Some(i)
      case _                   => None
    }
    val starting = 0 +: charLocations.map(_ + 1)
    val ending   = charLocations :+ string.length
    (starting, ending).zipped.map {
      case (start, end) => string.substring(start, end)
    }
  }

  def fromFile(file: File): Tutors = {
    val line = io.Source.fromFile(file).getLines()
    val numberOfSlots  = getFirstInt(line.next)
    val numberOfTutors = getFirstInt(line.next)
    val slotNames  = splitAtEachTab(line.next).tail.tail // skip 2 tabs
    assert(slotNames.size == numberOfSlots)
    val tutorNames = collection.mutable.MutableList.empty[String]
    val usernames  = collection.mutable.MutableList.empty[String]
    val tutorAvailability = Range(0, numberOfTutors).map {
      case i =>
        val username +: name +: avail = splitAtEachTab(line.next)
        usernames  += username
        tutorNames += name
        assert(avail.size <= numberOfSlots)
        avail.zipWithIndex.flatMap {
          case ("", i) => None
          case (_ , i) => Some(i) // nonempty string means okay
        }
    }
    Tutors(
      slotNames    = slotNames.toVector,
      tutorNames   = tutorNames.toVector,
      usernames    = usernames.toVector,
      availability = tutorAvailability)
  }

  // convert students to tutors
  def fromStudents(students: Seq[Student]): Tutors = {
    val slotNames    = config.slotNames
    val tutors       = students.toVector
    val tutorNames   = tutors.map(_.name)
    val usernames    = tutors.map(_.username)
    val availability = tutors.map(s => translateStudentAvailability(s.availability))
    Tutors(
      slotNames    = slotNames,
      tutorNames   = tutorNames,
      usernames    = usernames,
      availability = availability)
  }

  // convert student availability to tutor availability
  def translateStudentAvailability(avail: Seq[Boolean]): Seq[Int] =
    Range(0, avail.length).filter(avail)
}

case class Tutors (
  slotNames    : IndexedSeq[String],
  tutorNames   : IndexedSeq[String],
  usernames    : IndexedSeq[String],
  availability : IndexedSeq[Seq[Int]]
) {
  def formatSlotTutor(slot: Int, tutor: Int): String =
    s"${slotNames(slot)}-${usernames(tutor)}"
}
