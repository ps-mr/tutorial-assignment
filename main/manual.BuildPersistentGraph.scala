package manual

import data._
import tutorial.PersistentGraph

class BuildPersistentGraph(
  students          : Users,
  rooms             : Rooms,
  tutors            : Tutors,
  marginalRank      : IndexedSeq[Int],
  marginalCost      : IndexedSeq[Int],
  unassignedPenalty : Int
) extends Process[PersistentGraph] {
  def run(): PersistentGraph =
    reportTime("Building graph") {
      new PersistentGraph(students, rooms, tutors, marginalRank, marginalCost, unassignedPenalty)
    }
}
