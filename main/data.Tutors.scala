package data

import java.io.File
import Rooms.{datafile, getFirstInt} // todo: abstract?

object Tutors {
  lazy val dummy: Tutors = fromFile(datafile("tutors-dummy.txt"))

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
    val slotNames  = splitAtEachTab(line.next).tail
    assert(slotNames.size == numberOfSlots)
    val tutorNames = collection.mutable.MutableList.empty[String]
    val usernames  = collection.mutable.MutableList.empty[String]
    val tutorAvailability = Range(0, numberOfTutors).map {
      case i =>
        val username +: name +: avail = splitAtEachTab(line.next)
        usernames  += username
        tutorNames += name
        assert(avail.size == numberOfSlots)
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
}

case class Tutors (
  slotNames    : IndexedSeq[String],
  tutorNames   : IndexedSeq[String],
  usernames    : IndexedSeq[String],
  availability : IndexedSeq[Seq[Int]]
)
