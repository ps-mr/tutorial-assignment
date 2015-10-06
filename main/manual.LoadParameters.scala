package manual

/** @return (marginal_rank, marginal_cost, unassigned_penalty)
  */
object LoadParameters extends Process[(Vector[Int], Vector[Int], Int)] {
  def run(): (Vector[Int], Vector[Int], Int) =
    reportTime(s"Loading ${config.config("parameters")}") {
      val param = config.parameters
      ( param(config.marginal_rank),
        param(config.marginal_cost),
        param(config.unassigned_penalty).head )
    }
}

