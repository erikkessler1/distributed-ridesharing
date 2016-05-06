/**
 * Class that holds the state of the world for the simulator
 * and provides methods to display itself on the terminal.
 *
 * We assume a linear world that wraps around.
 * 
 * Erik Kessler and Kevin Persons
 */
class World(peers: List[Peer]) {

  // Height and width of the terminal window
  private val WIDTH = 132
  private val HEIGHT = 43

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
    print("=" * WIDTH)
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
    print(Command.commands.mkString("\n"))
  }


  /* METHODS FOR COMMAND HANDLING */

  private def handleCommands() = {
    for (ln <- io.Source.stdin.getLines) {

      val command = ln.split(" ")(0)
      val args = ln.split(" ").toList.tail

      // Execute the command and print the result
      val paddedResult = Command.execute(this, command, args).padTo(35, ' ')
      print("\n   " + ANSI.style(ANSI.BOLD::Nil, paddedResult))

      // Reset the prompt
      print(ANSI.up(2) + ANSI.delete + ANSI.restore + "-> ")
    }
  }

  /* METHODS FOR PRINTING THE WORLD */
   
  /**
   * Prints the updated positions of the world.
   */ 
  def printWorld() = { 

    // Move to correct place
    print(ANSI.move(4, 0))

    // Calculate the world line
    val line = makeWorldLine()
    print(line)

    // Print the log

    print(ANSI.restore + ANSI.right(2))
  }

  def makeWorldLine() = {
    val right = focusedPeer.pos + (WIDTH / 2)
    val left = focusedPeer.pos - (WIDTH / 2)
    val range = (left to right).map(n => (1000 + n) % 1000)

    val peersInRange = peers.filter(p => range.contains(p.pos))



    var line = ""
    for (i <- range) {
      //println(s"i: $i next: $next")

      line += (
      if (i == focusedPeer.pos) {
	ANSI.style(List(ANSI.PURPLE), "█") 
      } else if (peersInRange.exists(_.pos == i)) {
        ANSI.style(List(ANSI.GRAY), "█")
      } else 
	" ")
    }
    line
  }

  private def getNextPeer(peers: List[Peer]) = {
    peers.headOption match {
      case Some(p) => p.pos
      case None    => -1
    }
  }


  /* METHODS FOR MANIPULATING THE WORLD */

  // Peer to center the world around
  private var focusedPeer: Peer = peers(0);

  /**
   * Changes which peer the world is centered on.
   */ 
  def setFocus(n: Int) = {
    focusedPeer = peers(n)
    printWorld()
  }

  /**
   * Moves the world forward one step.
   */ 
  def step(steps: Int) = {
    for (i <- 1 to steps) {
      peers.foreach { _.step() }
      printWorld()
      if (steps > 1) Thread.sleep(200)
    }
  }

}
