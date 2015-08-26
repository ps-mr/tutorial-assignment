package minCostFlow

import optimus.optimization._

object Graph {
  type Vertex       = Int
  type Vertices     = IndexedSeq[Vertex]
  type Edge         = Int
  type Supply       = IndexedSeq[Int]
  type Edges        = IndexedSeq[(Vertex, Vertex)]
  type CostValue    = Int
  type Cost         = IndexedSeq[CostValue]
  type ReducedCost  = Cost
  type Capacity     = IndexedSeq[Int]
  type Flow         = IndexedSeq[Int]
  type Potential    = IndexedSeq[Int]
  type Excess       = IndexedSeq[Int]
  type Cut          = Seq[Int]

  def getSource(edge: Edge, edges: IndexedSeq[(Vertex, Vertex)]): Vertex = edges(edge)._1
  def getTarget(edge: Edge, edges: IndexedSeq[(Vertex, Vertex)]): Vertex = edges(edge)._2

  case class Simple(supply: Supply, edges: Edges, cost: Cost, capacity: Capacity) extends Graph
}

trait Graph {
  import Graph._

  val supply   : Supply
  val edges    : Edges
  val cost     : Cost
  val capacity : Capacity

  def createIntegerProgram(): (MIProblem, IndexedSeq[MPIntVar]) = {
    import optimus.algebra._

    implicit val problem = MIProblem(SolverLib.oJalgo)
    val flow: IndexedSeq[MPIntVar] = Range(0, edges.length).map {
      case i =>
        MPIntVar("flow" + i, Range(0, capacity(i) + 1))
    }

    val flowWithIndex = flow.zipWithIndex

    val capacityConstraints = for ((x, i) <- flowWithIndex) yield x <= capacity(i)

    val edgesWithIndex = edges.zipWithIndex

    val supplyConstraints = for {
      v <- 0 until supply.length
      outgoingFlow = (for { ((u, w), i) <- edgesWithIndex ; if u == v } yield flow(i)).fold[Expression](0)(_ + _)
      incomingFlow = (for { ((u, w), i) <- edgesWithIndex ; if w == v } yield flow(i)).fold[Expression](0)(_ + _)
    }
    yield outgoingFlow - incomingFlow := supply(v)

    val constraints = capacityConstraints ++ supplyConstraints

    //DEBUG
    println
    println("edges = " + edges.slice(36, 40))
    println("flow = " + flow.slice(36, 40))
    println(s"supply = ${supply.take(10)}")
    println(s"constraints\n" + supplyConstraints.take(10).map("  " + _).mkString("\n"))
    println

    minimize { (for ((x, i) <- flowWithIndex) yield x * cost(i)).fold[Expression](0)(_ + _) }
    subjectTo(constraints: _*)


    (problem, flow)
  }
}
