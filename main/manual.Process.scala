package manual

// Manual processes.
// Basically an IO monad specialized to command-line interactions.
trait Process[+A] {
  // may involve user interactions
  def run(): A

  // for for-comprehension
  def flatMap[B](other: A => Process[B]): Process[B] =
    Process.Bind(this, other)

  def map[B](f: A => B): Process[B] =
    Process.Bind(this, (x: A) => Process.Return(f(x)))

  // ignore filter results
  def withFilter(f: A => Any): Process[A] =
    for {
      res <- this
      ignored = f(res)
    }
    yield res

  // failure
  def fail(message: String): Nothing =
    throw Process.Fail(message)

  // failure due to scheduler bug
  def internalError(message: String): Nothing =
    throw Process.Fail(s"Scheduler internal error:\n$message")

  // timing report
  protected[this] def now(): Long =
    java.util.Calendar.getInstance.getTimeInMillis

  def reportTime[A](description: String)(job: => A): A = {
    print(s"$description...")
    Console.flush()
    val (result, elapsed) = timing(job)
    println(f"$done%s. ($elapsed%.2f s)")
    resetDone()
    result
  }

  // set the "done" message within one cycle of `reportTime`
  private[this] var done = "done"
  def resetDone(): Unit = { done = "done" }
  def setDone(msg: String): Unit = { done = msg }

  def timing[A](job: => A): (A, Double) = {
    val start   = now()
    val result  = job
    val end     = now()
    val elapsed = (end - start) / 1000.0
    (result, elapsed)
  }
}

object Process {
  def execute(proc: Process[Any]): Unit = {
    try { proc.run() }
    catch {
      case Fail(message) =>
        println
        println("================ Error ================")
        println(message)
        println("========== Execution failed. ==========")
    }
  }

  case class Return[+A](value: A) extends Process[A] {
    def run(): A = value
  }

  case class Bind[A, +B](prev: Process[A], next: A => Process[B]) extends Process[B] {
    def run(): B = {
      val res = prev.run()
      next(res).run
    }
  }

  case class Fail(message: String) extends Exception(message) with Process[Nothing] {
    def run(): Nothing = throw this
  }

  // verify a situation with user, fail unless user says yes
  def checkWithUser(question: String): Unit =
    chooseByUser(question)( () )( throw Process.Fail("Manual process cancelled by user.") )

  def chooseByUser[A](question: String)(thenBranch: => A)(elseBranch: => A): A = {
    print(s"$question (y/n) ")
    Console.flush()
    val line = scala.io.StdIn.readLine()
    if (line.startsWith("y") || line.startsWith("Y"))
      thenBranch
    else if (line.startsWith("n") || line.startsWith("N"))
      elseBranch
    else {
      println("Please answer yes or no.")
      chooseByUser(question)(thenBranch)(elseBranch)
    }
  }
}
