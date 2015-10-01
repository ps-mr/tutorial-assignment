package tutorial

import org.scalatest._
import data._

class PersistentGraphSpec
extends GraphSpecTrait[PersistentReport, PersistentGraph]("tutorial.PersistentGraph")
{
  def mkGraph = PersistentGraph.apply

  it should "format student assignment correctly" in {
    val Seq(g1, g2, g3, g4, g5) = report.assignedStudents.take(5).map(_.assignedGroup.get)
    assert(g1 == "monday_20-alec")
    assert(g2 == "monday_18-antonia")
    assert(g3 == "tuesday_08-abigail")
    assert(g4 == "monday_22-abraham")
    assert(g5 == "monday_18-antonia")
  }
}
