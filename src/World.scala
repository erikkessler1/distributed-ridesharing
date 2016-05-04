/**
 * Class that holds the state of the world for the simulator
 * and provides methods to display itself on the terminal.
 *
 * We assume a linear world that wraps around.
 * 
 * Erik Kessler and Kevin Persons
 */
class World(size: Int, peers: List[Peer]) {

  private val WIDTH = 132
  private val HEIGHT = 43

  // Peer to center the world around
  private var focusedPeer: Peer = null;

  /**
   * Setup the world and begin accepting commands to control the world.
   */ 
  def start() = {
    printInitialWorld()
    handleCommands()
  }

  /* METHODS FOR INITIAL TERMINAL SETUP */

  /**
   * Prints the template for the world.
   */
  private def printInitialWorld() = { 
    print(ANSI.clear)
    createWorldLine()
    createDividingLine()
    createLogArea()
    createCommandPrompt()
  }


  // Line of slots for the world.
  private def createWorldLine() = {
    print(ANSI.move(5,0))
    print("_" * WIDTH)
  }

  // Line between command prompt and log area
  private def createDividingLine() = {
    for (r <- 12 until HEIGHT) {
      print(ANSI.move(r, WIDTH/2) + "|")
    }
  }

  // Area for log entries for the current node.
  private def createLogArea() = {
    print(ANSI.move(12, WIDTH/2 + 1) + 
	  ANSI.style(List(ANSI.BOLD, ANSI.UNDERLINE), "Log for Current Peer:"))
  }

  // Command prompt for controlling the world.
  private def createCommandPrompt() = {
    print(ANSI.move(12, 0) + 
	  ANSI.style(List(ANSI.BOLD, ANSI.UNDERLINE), "Command Prompt:"))
    print(ANSI.move(14, 0))
    printInstructions()
    print(ANSI.down(1))
    print("\n")
    print(ANSI.store)
    print("-> ")
  }

  // Instructions for the command prompt
  private def printInstructions() = {
    print("s    : Step simulation\n" + 
	  "f [n]: Set focus on peer n")
  }


  /* METHODS FOR COMMAND HANDLING */

  private def handleCommands() = {
    for (ln <- io.Source.stdin.getLines) {

      val command = ln.split(" ")(0)
      val args = ln.split(" ").toList.tail

      print("\n   " + ANSI.style(ANSI.BOLD::Nil, Command.execute(command, args)))
      print(ANSI.up(2) + ANSI.delete + ANSI.restore + "-> ")
    }
  }

  /* METHODS FOR PRINTING THE WORLD */
   
  /**
   * Prints the updated positions of the world.
   */ 
  def printWorld() = { }

}


class Peer {

}
