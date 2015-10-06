package manual

object LoadRooms extends Process[data.Rooms] {
  def run(): data.Rooms =
    reportTime(s"Loading ${config.config("rooms")}") {
      data.Rooms.current // should Rooms.current be cached as a lazy val at all?
    }
}
