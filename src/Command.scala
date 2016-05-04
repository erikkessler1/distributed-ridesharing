/**
 * Provides a way to issue commands to a World.
 * 
 * Erik Kessler and Kevin Persons
 */ 
object Command {

  /**
   * All commands available
   */ 
  val commands = List(new StepCommand(), new FocusCommand())

  /**
   * Execute command, op, with args on the world
   */ 
  def execute(world: World, op: String, args: List[String]) = 
    // Find and execute the command that matches
    commands.find(_.c == op) match {
      case Some(command) => command.execute(world, args)
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

  def execute(world: World, args: List[String]): String

  override def toString() = s"${(c + " " + args).padTo(6, ' ')}: $description"

}

/**
 * Command for stepping the world forward one time unit.
 */ 
class StepCommand() extends WorldCommand("s", "", "Step simulation.") {

  override def execute(world: World, args: List[String]) = {
    world.step(1)
    "Step"
  }
}

/**
 * Command for setting the focus on a specific node.
 */ 
class FocusCommand() extends WorldCommand("f", "[n]", "Set focus on peer n.") {

  override def execute(world: World, args: List[String]) = {
    // Try to get the n
    val n = args match {
      case a::as => Util.toInt(a)
      case Nil   => None
    }

    n match {
      case Some(n) => world.setFocus(n); s"Focus set to peer $n"
      case None    => "Peer not found"
    }
  }
}
