package util

object WeekdayTranslator {
  val weekdayHourSeparator = '_'

  def germanTimeslot(tuesday_08: String): String = {
    val (weekday, _hour) = tuesday_08.span(_ != weekdayHourSeparator)
    s"${germanWeekday(weekday)} ${germanHour(_hour.tail)}"
  }

  def germanWeekday(tuesday: String): String =
    tuesday.toLowerCase match {
      case "monday"    => "Montag"
      case "tuesday"   => "Dienstag"
      case "wednesday" => "Mittwoch"
      case "thursday"   => "Donnerstag"
      case "friday"    => "Freitag"
      case other       => sys error s"not a weekday: $other"
    }

  def germanHour(x08: String): String =
    s"$x08.00 Uhr"
}
