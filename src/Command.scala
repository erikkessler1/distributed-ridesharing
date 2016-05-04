object Command {

  val commands = List(new StepCommand(), new FocusCommand())

  def execute(world: World, op: String, args: List[String]) = 
    commands.find(_.c == op) match {
      case Some(command) => command.execute(world, args)
      case None          => "Command not understood"
    }

}

abstract class WorldCommand(val c: String, val args: String, val description: String) { 

  def execute(world: World, args: List[String]): String

  override def toString() = s"${(c + " " + args).padTo(6, ' ')}: $description"

}

class StepCommand() extends WorldCommand("s", "", "Step simulation.") {

  override def execute(world: World, args: List[String]) = {
    world.step(1)
    "Step"
  }
}

class FocusCommand() extends WorldCommand("f", "[n]", "Set focus on peer n.") {

  override def execute(world: World, args: List[String]) = {
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
