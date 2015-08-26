/** Transforming a balls-into-bins problem into a min-cost-flow problem.
  *
  * Problem BALLS-INTO-RECYCLE-BINS:
  *
  * Overview
  * - Goal: sort n balls of composite garbage into m recycle bins for
  *   specific materials, to minimize cost due to mis-sorted garbage
  *   and overflowing garbage bins.
  * - Each ball can be put into a limited subset of bins. E. g., a plastic
  *   ball may not be put into the organic garbage bin.
  * - Each ball-bin combination has a cost. For example, if a ball is made
  *   of 70% plastic and 30% glass, then it costs more to put it in the
  *   glass recycle bin than in the plastic recycle bin.
  *
  * Instance
  * - n, the number of balls (not a parameter of `Graph`)
  * - m, the number of bins
  * - a mapping from balls to sequences of allowed bins and associated cost
  * - capacity of overflowing bins as a sequence of integers. E. g., the
  *   sequence (10, 5, 2) may mean that the bin holds the first 10 balls
  *   comfortably, that it can hold 5 more balls with some effort, and that
  *   one can squeeze 2 more balls in there if one tries hard enough.
  * - cost of overflowing bins as a sequence of integers. E. g., the
  *   sequence (0, 3, 9) may mean that the first 10 balls have no cost,
  *   the next 5 balls cost 3 unit of effort each to put in, and the last
  *   2 balls cost 9 unit of effort each.
  * - an integer quantifying how undesirable it is to not put a ball in any
  *   bin. usually rather high.
  *
  * Transformation
  * To transform BALLS-INTO-RECYCLE-BINS into a min-cost-flow instance:
  * - Balls and bins become vertices. Balls are sources of 1 unit of flow.
  *   There are arcs from each ball to its permitted bins, with cost equal
  *   to the cost of putting this ball into that bin.
  * - Ranks of bin-overflow become vertices. Each bin is connected to a
  *   rank by an arc of capacity equal to its marginal capacity at this
  *   level of overflow. The cost of the arc is the cost of putting in
  *   a ball at this level of overflow.
  * - Ranks are connected to the sink via zero-cost edges with essentially
  *   infinite capacity.
  * - A distinguished vertex "hell" is added to ensure the existence of a
  *   feasible flow, the assumption of many min-cost-flow-algorithms. Balls
  *   are connected to "hell" through a high-cost edge, and "hell" is
  *   connected to the sink via a zero-cost edge with essentially infinite
  *   capacity.
  */

package ballsAndBins

import minCostFlow.Relaxation._

object Graph {
  case class BallIntoBin(ball: Vertex, bin: Vertex, cost: CostValue)
}

class Graph (
  val numberOfBins   : Int,
  val binsForBalls   : IndexedSeq[Seq[(Int, CostValue)]],
  val marginalRank   : Seq[Int], // bracket size for marginal tax on bin size
  val marginalCost   : Seq[Int],
  val binlessPenalty : Int
) {
  import Graph.BallIntoBin

  val numberOfBalls: Int = binsForBalls.length
  val numberOfRanks: Int = marginalRank.size
  assert(numberOfRanks == marginalCost.size)

  val ballSupply : Supply = Vector.fill(numberOfBalls)(1)
  val binSupply  : Supply = Vector.fill(numberOfBins )(0)
  val rankSupply : Supply = Vector.fill(numberOfRanks)(0)
  val hellSupply : Supply = Vector(0)
  val sinkSupply : Supply = Vector(-numberOfBalls)

  val supply: Supply = ballSupply ++ binSupply ++ rankSupply ++ hellSupply ++ sinkSupply

  val balls: Vertices = Range(0, numberOfBalls)
  val bins : Vertices = Range(balls.last + 1, balls.last + 1 + numberOfBins)
  val ranks: Vertices = Range(bins .last + 1, bins .last + 1 + numberOfRanks)
  val hell : Vertex   = ranks.last + 1
  val sink : Vertex   = hell + 1
  assert(sink + 1 == supply.length)

  val childrenOfBall: IndexedSeq[Seq[BallIntoBin]] =
    for { (binIndices, ball) <- binsForBalls.zipWithIndex } yield
      (for { (binIndex, cost) <- binIndices } yield
        BallIntoBin(ball, bins(binIndex), cost)) :+ BallIntoBin(ball, hell, binlessPenalty)

  val edgesFromBalls : Edges    = childrenOfBall.flatMap(_.map(bb => (bb.ball, bb.bin)))
  val capaFromBalls  : Capacity = edgesFromBalls.map(_ => 1)
  val costFromBalls  : Cost     = childrenOfBall.flatMap(_.map(_.cost))

  val edgesFromBins  : Edges    = for { bin <- bins ; rank <- ranks } yield (bin, rank)
  val capaFromBins   : Capacity = bins.flatMap(_ => marginalRank)
  val costFromBins   : Cost     = bins.flatMap(_ => marginalCost)

  val edgesToSink    : Edges    = for { v <- ranks.head to hell } yield (v, sink)
  val capaToSink     : Capacity = edgesToSink.map(_ => numberOfBalls)
  val costToSink     : Cost     = edgesToSink.map(_ => 0)

  val edges    = edgesFromBalls ++ edgesFromBins ++ edgesToSink
  val capacity = capaFromBalls  ++ capaFromBins  ++ capaToSink
  val cost     = costFromBalls  ++ costFromBins  ++ costToSink

  def computeFlow(): Flow =
    minCostFlow.Relaxation.computeFlow(supply, edges, cost, capacity)
}

