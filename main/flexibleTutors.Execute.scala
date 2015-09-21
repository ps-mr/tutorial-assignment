/** sort last year's students into groups using flexibleTutors graph */
package flexibleTutors

import minCostFlow.Graph._

object Execute {
  sealed trait Mode
  case object Relax extends Mode
  case object IProg extends Mode
  case object Activ extends Mode

  def main(args: Array[String]): Unit = {

    val mode = args match {
      case Array("IntegerProgram") => IProg
      case Array("withActivation") => Activ
      case _                       => Relax
    }

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

    mode match {
      case Activ =>
        val timeInit = now

        val marginalRank      = Seq(1, 1, 1,  1,  1)
        val marginalCost      = Seq(1, 3, 6, 10, 15)
        val unassignedPenalty = 5000

        val users  = data.Users.year2014
        val rooms  = data.Rooms.dummy
        val tutors = data.Tutors.dummy

        reportTime(timeInit, "initialization and IO")

        val timeConstruct = now()
        val graph = new tutorial.Graph(users, rooms, tutors, marginalRank, marginalCost, unassignedPenalty)

        reportTime(timeConstruct, "graph construction")

        val timeCompute = now
        val report = graph.computeReport()

        reportTime(timeCompute, "flow computation")

        println(report.forHuman(rooms, tutors))

      case Relax | IProg =>

        val solver: minCostFlow.Solver = mode match {
          case Relax => minCostFlow.Relaxation
          case _     => minCostFlow.IntegerProgram
        }

        val timeInit = now

        val choicePenalty = Seq(0, 3, 6)
        val groupCapacity = Seq(50, 10, 10, 10)
        val groupSizeCost = Seq( 0, 15, 30, 90)
        val grouplessCost = 5000

        val users = data.Users.year2014

        reportTime(timeInit, "initialization and IO")


        val timeConstruct = now()

        val graph = flexibleTutors.Graph(users, choicePenalty, groupCapacity, groupSizeCost, grouplessCost)

        reportTime(timeConstruct, "graph construction")

        val timeCompute = now
        val report = graph.computeReport(solver = solver)
        val flow = report.flow
        reportTime(timeCompute, "flow computation")

        println()
        println(report.choiceStatistics)
        println()
        println(report.groupSizeStatistics(30, 40, 50, 60))
        println()
    }
  }
}
