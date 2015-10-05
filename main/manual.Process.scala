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

  // failure
  def fail(message: String): Nothing =
    throw Process.Fail(message)

  // failure due to scheduler bug
  def internalError(message: String): Nothing =
    throw Process.Fail(s"Scheduler internal error:\n$message")

  // timing report
  private[this] def now(): Long =
    java.util.Calendar.getInstance.getTimeInMillis

  def reportTime[A](description: String)(job: => A): A = {
    print(s"$description...")
    Console.flush()
    val (result, elapsed) = timing(job)
    println(f"done. ($elapsed%.2f s)")
    result
  }

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
    def run(): B = next(prev.run()).run()
  }

  case class Fail(message: String) extends Exception(message) with Process[Nothing] {
    def run(): Nothing = throw this
  }
}
