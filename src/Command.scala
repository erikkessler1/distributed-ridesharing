/**
 * Provides a way to issue commands to a World.
 * Makes creating new commands easy - extend WorldCommand and implement execute.
 *
 * Erik Kessler and Kevin Persons
 */
object Command {

  /**
   * All commands available
   */
  val commands: List[WorldCommand] = List(new StepCommand(), new FocusCommand(), new MessagesCommand(), new RidesCommand(), new PartitionCommand())

  /**
   * Execute command, op, with args on the world
   */
  def execute(op: String, args: List[String]) = commands.find(_.c == op) match {
                                                  case Some(command) => command.execute(args)
                                                  case None          => "Command not understood"
                                                }
}

/**
 * Class for encapulating a command. c is the command, args are the arguement
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

    // Get the number of steps to move
    val n = args match {
      case n::as => Util.toInt(n).getOrElse(1)
      case Nil   => 1
    }

    // Get the delay between steps
    val delay = args match {
      case _::d::as => Util.toInt(d).getOrElse(200)
      case _        => 200
    }

    // Execute the step
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

    // Set the focus on that peer or report an error
    n match {
      case Some(n) => World.setFocus(n); s"Focus set to peer $n"
      case None    => "Peer not found"
    }
  }
}

/**
 * Command that prints the total number of network messages sent since
 * the start of the simulation.
 */
class MessagesCommand() extends WorldCommand("m", "", "Print total messages sent.") {

  override def execute(args: List[String]) = {
    Network.sentMessages + " total messages sent."
  }
}

/**
 * Command that prints the number of matched rides over the total number
 * of rides requested since the start of the simulation.
 */
class RidesCommand() extends WorldCommand("r", "", "Print ratio of ride matching.") {

  override def execute(args: List[String]) = {
    s"${Network.rideMatches}/${Network.rideRequests} of rides requests serviced"
  }
}


class PartitionCommand() extends WorldCommand("p", "", "Print whether network has become partitioned.") {

    override def execute(args: List[String]) = {
      val partitions = Simulator.findNetworkPartitions()
      s"The network is partitioned into ${partitions} section(s)."
    }
}
