/**
 * Provides a way to issue commands to a World.
 *
 * Erik Kessler and Kevin Persons
 */
object Command {

  /**
   * All commands available
   */
  val commands: List[WorldCommand] = List(new StepCommand(), new FocusCommand(), new MessagesCommand(), new RidesCommand())

  /**
   * Execute command, op, with args on the world
   */
  def execute(op: String, args: List[String]) =
    // Find and execute the command that matches
    commands.find(_.c == op) match {
      case Some(command) => command.execute(args)
      case None          => "Command not understood"
    }

}

/**
 * Class for encapulating a command. C is the command, args are the arguement
 * options, and description describes it.
 *
 * Concrete classes implement the execute method which defines whath the command
 * does.
 */
abstract class WorldCommand(val c: String, val args: String, val description: String) {

  def execute(args: List[String]): String

  override def toString() = s"${(c + " " + args).padTo(6, ' ')}: $description"

}

/**
 * Command for stepping the world forward one time unit.
 */
class StepCommand() extends WorldCommand("s", "[n] [d]", "Step simulation n steps with d delay.") {

  override def execute(args: List[String]) = {
    val n = args match {
      case n::as => Util.toInt(n).getOrElse(1)
      case Nil   => 1
    }

    val delay = args match {
      case _::d::as => Util.toInt(d).getOrElse(200)
      case _        => 200
    }

    World.step(n, delay)
    "Step"
  }
}

/**
 * Command for setting the focus on a specific node.
 */
class FocusCommand() extends WorldCommand("f", "[n]", "Set focus on peer n.") {

  override def execute(args: List[String]) = {
    // Try to get the n
    val n = args match {
      case a::as => Util.toInt(a)
      case Nil   => None
    }

    n match {
      case Some(n) => World.setFocus(n); s"Focus set to peer $n"
      case None    => "Peer not found"
    }
  }
}

class MessagesCommand() extends WorldCommand("m", "", "Print total messages sent.") {

  override def execute(args: List[String]) = {
    Util.sentMessages + " total messages sent."
  }
}

class RidesCommand() extends WorldCommand("r", "", "Print ratio of ride matching.") {

  override def execute(args: List[String]) = {
    s"${Util.matches}/${Util.rideRequests} of rides requests serviced"
  }
}
