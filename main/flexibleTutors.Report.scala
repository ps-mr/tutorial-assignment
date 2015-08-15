/** Assume possibility to distribute tutors over groups no matter what */
package flexibleTutors

import minCostFlow.Relaxation._
import collection.mutable.MutableList

case class Report (
  /** number of students                */ students : Int,
  /** number of groups                  */ groups   : Int,
  /** number of choices excluding hell  */ choices  : Int,
  /** edges of the flexibleTutors graph */ edges    : Edges,
  /** minimum cost flow                 */ flow     : Flow
) {
  val outDegree = choices + 1

  //val prefEdges = flow.slice(0, outDegree * students)


  /** find out which group a student is assigned to */
  def groupOf(student: Vertex): Option[Int] = {
    Range(outDegree * student, outDegree * (student + 1)).find({
      case e => flow(e) == 1
    }).map(e => getTarget(e, edges) - students)
  }

  /** find out which choice a student is assigned to */
  def choiceOf(student: Vertex): Option[Int] = {
    flow.slice(outDegree * student, outDegree * (student + 1)).indexWhere(_ == 1) match {
      case i if i < choices => Some(i)
      case _                => None
    }
  }

  val studentsInGroup: Seq[Seq[Vertex]] = {
    val studentsInGroup: IndexedSeq[MutableList[Vertex]] = Array.fill(groups)(MutableList.empty[Vertex])
    for (s <- Range(0, students) ; i <- groupOf(s))
      studentsInGroup(i) += s
    studentsInGroup
  }

  val studentsWithChoice: Seq[Seq[Vertex]] = {
    val studentsWithChoice: IndexedSeq[MutableList[Vertex]] = Array.fill(choices)(MutableList.empty[Vertex])
    for (s <- Range(0, students) ; i <- choiceOf(s))
      studentsWithChoice(i) += s
    studentsWithChoice
  }

  val unassignedStudents: Seq[Vertex] =
    Range(0, students).filter(s => choiceOf(s) == None)

  /** how many has first choice, how many unassigned */
  val choiceStatistics: String =
    (Range(0, choices).map({
      case i =>
        val nth = ordinal(i + 1)
        percent(studentsWithChoice(i).size, students, s"assigned to $nth choice")
    }) :+ percent(unassignedStudents.size, students, "unassigned")
    ).mkString("\n")

  case class GroupSizeClass(min: Int, max: Int, size: Int)

  def classifyGroupSize(bounds: Int*): Seq[GroupSizeClass] = {
    val augBounds = -1 +: bounds :+ Int.MaxValue
    Range(1, augBounds.size).map {
      case i =>
        val min  = augBounds(i - 1) + 1
        val max  = augBounds(i)
        val size = studentsInGroup.count(group => min < group.size & group.size <= max)
        GroupSizeClass(min, max, size)
    }
  }

  def groupSizeStatistics(bounds: Int*): String =
    classifyGroupSize(bounds: _*).map({
      case GroupSizeClass(min, max, size) =>
        val formatString =
          if (max == Int.MaxValue)
            s"groups of size at least $min"
          else if (min <= 0)
            s"groups of size at most $max"
          else
            f"groups of size $min%2d--$max%2d"
        percent(size, groups, formatString)
    }).mkString("\n")

  /** convert integer to "0th", "1st", "2nd", "3rd", "11th", etc.
    * no guarantee for negative integers.
    */
  def ordinal(i: Int): String =
    if (i % 100 / 10 == 1)
      i.toString + "th"
    else
      i.toString + (i % 10 match {
        case 1 => "st"
        case 2 => "nd"
        case 3 => "rd"
        case _ => "th"
      })

  def percent(numerator: Int, denominator: Int, thing: String): String = {
    val percentage =
      if (denominator != 0)
        f"(${Math.round(100.0 * numerator / denominator)}%02d%%)"
      else
        s"(--%)"
    f"$numerator%5d $percentage%s  $thing"
  }
}
