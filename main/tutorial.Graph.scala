/** Transforming a tutorial scheduling problem into
  * a min-cost-flow-with-activation problem.
  *
  *
  * PROBLEM tutorial-scheduling
  *
  * Instance
  * - r classrooms
  * - t time slots, each with a different set of available rooms
  * - s students, each with a different set of variously preferred time slots
  * - u tutors, each with a different set of free time slots
  *
  * Output
  * Put student and tutors into rooms such that
  * - each room has 1 tutor
  * - the distribution of students is relatively uniform
  * - most students get their preferred time slot
  *
  *
  * TRANSFORMATION
  *
  * The transformation starts as a balls-and-bins transformation with
  * students as balls and time slots as bins. After that, all edges
  * between bins and ranks are deleted, and vertices are added to
  * symbolize tutors. Each bin (time slot) are joined
  * to tutors available at that time. Tutors are joined to ranks as in
  * the balls-and-bins transformation to make tutorial sizes even.
  *
  * The activation edges are the edges between time slots and tutors.
  * The equality constraint makes sure that each tutor is assigned
  * to exactly one time slot. The inequality constraint makes sure
  * that a time slot is never assigned to more rooms than there are
  * tutors for.
  */

package tutorial

import minCostFlow.Graph._
import minCostFlow.withActivation.Graph.Matrix

object Graph {
  def apply(
    users             : data.Users,
    rooms             : data.Rooms,
    tutors            : data.Tutors,
    marginalRank      : Seq[Int],
    marginalCost      : Seq[Int],
    unassignedPenalty : Int
  ): Graph = new Graph (
    roomsPerSlot        = rooms.roomsPerSlot,
    studentAvailability = users.preferences,
    tutorAvailability   = tutors.availability,
    marginalRank        = marginalRank,
    marginalCost        = marginalCost,
    unassignedPenalty   = unassignedPenalty
  )
}

class Graph (
  roomsPerSlot        : IndexedSeq[Int], // map time slot index to room number
  studentAvailability : IndexedSeq[Seq[Int]],
  tutorAvailability   : IndexedSeq[Seq[Int]],
  marginalRank        : Seq[Int], // does not include first-level rank
  marginalCost        : Seq[Int], // does not include first-level cost
  unassignedPenalty   : Int
) extends minCostFlow.withActivation.Graph {

  val numberOfTimeSlots    : Int = roomsPerSlot.length
  val numberOfStudents     : Int = studentAvailability.length
  val numberOfTutors       : Int = tutorAvailability.length
  def startingTutorialSize : Int = numberOfStudents / numberOfTutors
  def startingTutorialCost : Int = 0

  val augmentedMarginalRank = startingTutorialSize +: marginalRank
  val augmentedMarginalCost = startingTutorialCost +: marginalCost

  private[this]
  val bnb: ballsAndBins.Graph = new ballsAndBins.Graph (
    numberOfBins   = numberOfTimeSlots,
    binsForBalls   = studentAvailability.map(_.map(slot => (slot, 0))),
    marginalRank   = augmentedMarginalRank,
    marginalCost   = augmentedMarginalCost,
    binlessPenalty = unassignedPenalty
  )

  val students = bnb.balls
  val slots    = bnb.bins
  val ranks    = bnb.ranks
  val hell     = bnb.hell
  val sink     = bnb.sink
  val tutors   = Range(bnb.supply.length, bnb.supply.length + numberOfTutors)

  val tutorSupply: Supply = Vector.fill(numberOfTutors)(0)
  val supply: Supply = bnb.supply ++ tutorSupply
  assert(supply.length == tutors.last + 1)

  val edgesFromStudents = bnb.edgesFromBalls
  val capaFromStudents  = bnb.capaFromBalls
  val costFromStudents  = bnb.costFromBalls

  val edgesFromSlots =
    for {
      (avail, tutorIndex) <- tutorAvailability.zipWithIndex
      slotIndex <- avail
    }
    yield (slots(slotIndex), tutors(tutorIndex))

  val capaFromSlots = edgesFromSlots.map(_ => numberOfStudents)
  val costFromSlots = edgesFromSlots.map(_ => 0)

  val edgesFromTutors = for { tutor <- tutors ; rank <- ranks } yield (tutor, rank)
  val capaFromTutors  = tutors.flatMap(_ => augmentedMarginalRank)
  val costFromTutors  = tutors.flatMap(_ => augmentedMarginalCost)

  val edgesToSink = bnb.edgesToSink
  val capaToSink  = bnb.capaToSink
  val costToSink  = bnb.costToSink

  val edges    : Edges     = edgesFromStudents ++ edgesFromSlots ++ edgesFromTutors ++ edgesToSink
  val cost     : Cost      = costFromStudents  ++ costFromSlots  ++ costFromTutors  ++ costToSink
  val capacity : Capacity  = capaFromStudents  ++ capaFromSlots  ++ capaFromTutors  ++ capaToSink

  // activation edges are those between slots and tutors
  val activationEdges = Range (
    edgesFromStudents.length,
    edgesFromStudents.length + edgesFromSlots.length
  )

  // every tutor has exactly one slot
  val activationEqMatrix : Matrix = Matrix(tutors.map {
    case tutor => activationEdges.map(e => if (getTarget(e, edges) == tutor) 1 else 0)
  })
  val activationEqResult : IndexedSeq[Int] = tutors.map(_ => 1)

  // no slot has more than its share of rooms
  val activationLeMatrix : Matrix = Matrix(slots.map {
    case slot => activationEdges.map(e => if (getSource(e, edges) == slot) 1 else 0)
  })

  val activationLeResult : IndexedSeq[Int] = roomsPerSlot

  def computeReport(): Report =
    Report(this, minCostFlow.IntegerProgram.computeActivatedFlow(this))
}
