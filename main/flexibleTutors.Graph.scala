/** - Create min-cost-flow instance from students' preferences of tutorials.
  *
  * - Does not consider tutors' availability, assume the possibility to
  *   distribute tutors to groups no matter what.
  *
  * - There is a "marginal tax" on group size: the bigger the group is,
  *   the more it costs to add a student to it.
  *
  * - Each student has to make a fixed number of choices (3 for last year).
  *   We have to make sure all students have the same number of choices,
  *   because the choice of people with fewer choices count more in an
  *   unfair way. In particular, if a student makes only 1 choice, then
  *   he forces the system to put him in that group.
  *
  * The flexibleTutors graph looks like this.
  *
  * Vertices:
  * - one vertex per student with supply 1
  * - one vertex per group with supply 0
  * - one vertex per bracket of marginal tax
  * - a distinguished vertex "hell" for unassigned students
  * - a distinguished vertex "goal" with deficit = number of students
  *
  * Edges:
  * - arcs from students to their choices with capacity 1 and cost
  *   determined by the penalty of taking 1st, 2nd, 3rd choices etc.
  * - arcs from groups to the marginal tax brackets, with
  *   capacity = bracket size and cost = marginal cost per student
  * - arcs from marginal tax brackets to "goal" with
  *   capacity = number of students and cost = 0
  * - arcs from students to "hell" with capacity = 1 and a very large
  *   cost
  * - arc from "hell" to "goal" with capacity = number of students
  *   and cost = 0
  */
package flexibleTutors

class Graph (
  /** the number of groups                        */ groups        : Int,
  /** array of preferred slots                    */ preferences   : IndexedSeq[Seq[Int]],
  /** cost of taking 1st, 2nd, 3rd choice etc.    */ choicePenalty : Seq[Int],
  /** bracket sizes of marginal tax on group size */ groupBrackets : Seq[Int],
  /** bracket rates of marginal tax on group size */ groupSizeCost : Seq[Int],
  /** how much we hate unassigned students        */ grouplessCost : Int
) extends ballsAndBins.Graph (
  numberOfBins   = groups,
  binsForBalls   = preferences.map(_ zip choicePenalty),
  marginalRank   = groupBrackets,
  marginalCost   = groupSizeCost,
  binlessPenalty = grouplessCost
) {
  /** compute min cost flow and generate a report about it */
  def computeReport(solver: minCostFlow.Solver): Report =
    Report(
      students = numberOfBalls,
      groups   = numberOfBins,
      choices  = choicePenalty.size,
      edges    = edges,
      flow     = solver.computeFlow(this)
    )
}
