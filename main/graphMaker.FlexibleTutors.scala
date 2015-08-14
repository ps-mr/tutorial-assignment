package graphMaker

/** - create min-cost-flow instance from students' preferences of tutorials
  *
  * - does not consider tutors' availability, assume the possibility to
  *   distribute tutors to groups no matter what
  */
case class FlexibleTutors (
  /** the number of groups                        */ groups        : Int,
  /** array of preferred slots                    */ preferences   : IndexedSeq[Seq[Int]],
  /** cost of taking 1st, 2nd, 3rd choice etc.    */ choicePenalty : Seq[Int],
  /** bracket sizes of marginal tax on group size */ groupBrackets : Seq[Int],
  /** bracket rates of marginal tax on group size */ groupSizeCost : Seq[Int],
  /** how much we hate unassigned students        */ grouplessCost : Int
) {
  import minCostFlow.Relaxation._

  val students: Int = preferences.size

  val studentSupply: Supply = Vector.fill(students)(1)
  val groupSupply  : Supply = Vector.fill(groups  )(0)
  val sortingSupply: Supply = Vector(0, 0, 0, 0) // good, okay, bad, evil
                                                 // or: Gryphindor, Ravenclaw, Hufflepuff, Slytherine
  val hellSupply   : Supply = Vector(0)
  val goalSupply   : Supply = Vector(-students)
  val supply       : Supply = studentSupply ++ groupSupply ++ sortingSupply ++ hellSupply ++ goalSupply

  val groupIndices: IndexedSeq[Vertex] = Range(students, students + groups).toVector

  // the sink of all flow
  val goal: Vertex = supply.size - 1

  // unassigned students go through hell
  val hell: Vertex = goal - 1

  val brackets: IndexedSeq[Vertex] = Range(students + groups, hell).toVector
  assert(brackets.size == groupSizeCost.size)
  assert(brackets.size == groupBrackets.size)

  // preferences augmented with hell
  val augPreferences = preferences.map(_ :+ hell)

  // cost of choices augmented by distaste for hell
  val augChoicePenalty = choicePenalty :+ grouplessCost

  // edges from students to their chosen groups and to hell
  val prefEdges: Edges = for {
    (pref, i) <- augPreferences.zipWithIndex
    j         <- pref
  } yield (i, j)
  val prefCapa: Capacity = prefEdges.map(_ => 1)
  val prefCost: Cost     = augPreferences.flatMap(_ => augChoicePenalty)

  // edges from groups to the brackets of marginal size tax
  val bracEdges: Edges    = for { group <- groupIndices ; bracket <- brackets } yield (group, bracket)
  val bracCapa : Capacity = groupIndices.flatMap(_ => groupBrackets)
  val bracCost : Cost     = groupIndices.flatMap(_ => groupSizeCost)

  // edges from tax brackets and hell to the sink
  val goalEdges: Edges    = for { i <- brackets :+ hell } yield (i, goal)
  val goalCapa : Capacity = goalEdges.map(_ => students)
  val goalCost : Cost     = goalEdges.map(_ => 0)

  val edges    = prefEdges ++ bracEdges ++ goalEdges
  val capacity = prefCapa  ++ bracCapa  ++ goalCapa
  val cost     = prefCost  ++ bracCost  ++ goalCost
}
