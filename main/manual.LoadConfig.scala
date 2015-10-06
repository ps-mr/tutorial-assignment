package manual

object LoadConfig extends Process[Unit] {
  def run(): Unit = {
    import config.{config => c, _}
    reportTime(s"Loading ${config.configFile}") { val x = c           }
    reportTime(s"Loading ${c("credential")  }") { val x = credential  }
    reportTime(s"Loading ${c("assigned_at") }") { val x = assigned_at }
  }
}
