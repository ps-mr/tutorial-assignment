package data

import java.io.File

object Rooms {
  def datafile(relativePath: String): File =
    new File(getClass.getResource("").getPath, s"../../../../data/$relativePath")

  def getFirstInt(string: String): Int = {
    val start = string.indexWhere(_.isDigit)
    val end   = {
      val endIndex = string.indexWhere(! _.isDigit, start)
      if (endIndex < 0) string.length else endIndex
    }
    string.substring(start, end).toInt
  }

  lazy val dummy: Rooms = fromFile(datafile("rooms-dummy.txt"))

  def fromFile(file: File): Rooms = {
    val line = io.Source.fromFile(file).getLines()

    val numberOfSlots = getFirstInt(line.next)
    val numberOfRooms = getFirstInt(line.next)

    val slotNames = collection.mutable.MutableList.empty[String]

    val roomNames: IndexedSeq[Seq[String]] = Range(0, numberOfSlots).map { _ =>
      val Array(slotName, rest) = line.next.split(':')
      slotNames += slotName.trim
      rest.trim.split(",\\s*"): Seq[String]
    }

    val roomsPerSlot: IndexedSeq[Int] = roomNames.map(_.size)

    Rooms(slotNames.toVector, roomNames, roomsPerSlot)
  }
}

case class Rooms (
  slotNames: IndexedSeq[String],
  roomNames: IndexedSeq[Seq[String]],
  roomsPerSlot: IndexedSeq[Int]
)