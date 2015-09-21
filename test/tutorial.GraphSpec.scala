package tutorial

import org.scalatest._

class GraphSpec extends GraphSpecTrait("tutorial.Graph") {
  it should "sort all tutors and students" in {
    assert(report.unassignedStudents.isEmpty)
    assert(report.unassignedTutors.isEmpty)
  }

  it should "make group sizes as even as possible" in {
    report.groupSizeOfTutor.foreach {
      case size =>
        assert(size >= 12)
        assert(size <= 13)
    }
  }
}

abstract class GraphSpecTrait(name: String) extends FlatSpec {
  var graph : Graph  = null
  var report: Report = null

  name should "classify last year's data without crashing" in {
    val marginalRank      = Seq(1, 1, 1,  1,  1)
    val marginalCost      = Seq(1, 3, 6, 10, 15)
    val unassignedPenalty = 5000

    val users  = data.Users.year2014
    val rooms  = data.Rooms.dummy
    val tutors = data.Tutors.dummy

    graph = new Graph(users, rooms, tutors, marginalRank, marginalCost, unassignedPenalty)
    report = graph.computeReport()
  }
}
