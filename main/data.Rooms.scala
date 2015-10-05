package data

import java.io.File
import config.{dataFile, getFirstInt}
import spray.json._
import DefaultJsonProtocol._

object Rooms {
  lazy val dummy: Rooms = fromFile(dataFile("rooms-dummy.txt"))

  lazy val current: Rooms = fromMap(config.rooms)

  def fromMap(jsObj: Map[String, Vector[String]]): Rooms = {
    val slotNames = jsObj(config.timeslots)
    val roomNames: IndexedSeq[Seq[String]] = slotNames.map(jsObj)
    val roomsPerSlot = roomNames.map(_.size)
    Rooms(slotNames, roomNames, roomsPerSlot)
  }

  def fromFile(file: File): Rooms =
    // type annotation mandatory
    fromMap(config.fromJsonFile[Map[String, Vector[String]]](file))
}

case class Rooms(
  slotNames: IndexedSeq[String],
  roomNames: IndexedSeq[Seq[String]],
  roomsPerSlot: IndexedSeq[Int]
)
