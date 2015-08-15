/** sort last year's students into groups using flexibleTutors graph */
package flexibleTutors

import minCostFlow.Relaxation._

object Execute {
  def main(args: Array[String]): Unit = {
    def now() = java.util.Calendar.getInstance.getTimeInMillis
    def elapse(last: Long): String = f"${now - last}%8d ms"
    def reportTime(last: Long, job: String) = println(s"${elapse(last)}  $job")

    def percent(numerator: Int, denominator: Int, thing: String) = {
      val percent =
        if (denominator != 0)
          f"(${Math.round(100.0 * numerator / denominator)}%02d%%)"
        else
          s"(--%)"
      println(f"$numerator%5d $percent%s  $thing")
    }
    println()

    val timeInit = now

    val choicePenalty = Seq(0, 3, 6)
    val groupCapacity = Seq(50, 10, 10, 10)
    val groupSizeCost = Seq( 0, 15, 30, 90)
    val grouplessCost = 5000

    val file = new java.io.File(getClass.getResource("").getPath,
      "../../../../data/group-prefs-2014.txt")

    val line = io.Source.fromFile(file).getLines()
    val students = line.next().filter(_.isDigit).toInt
    val tutors   = line.next().filter(_.isDigit).toInt

    val preferences: IndexedSeq[Seq[Vertex]] =
      Vector.fill(students) {
        val prefs = (line.next().split(Array(' ', '\t')).map(_.toInt): Seq[Vertex])
        assert(prefs.size == choicePenalty.size)
        prefs
      }

    reportTime(timeInit, "initialization and IO")

    val timeConstruct = now()

    val graph = flexibleTutors.Graph(
      tutors, preferences, choicePenalty, groupCapacity, groupSizeCost, grouplessCost)
    import graph._

    val List(good, okay, bad, evil, hell, goal) = Range(students + tutors, supply.size).toList
    val sorts = Vector(good, okay, bad, evil)
    assert(sorts.size == groupSizeCost.size)
    assert(sorts.size == groupCapacity.size)

    reportTime(timeConstruct, "graph construction")

    val timeCompute = now
    val report = graph.computeReport
    val flow = report.flow
    reportTime(timeCompute, "flow computation")

    println()
    println(report.choiceStatistics)
    println()
    println(report.groupSizeStatistics(30, 40, 50, 60))
    println()

  }
}
