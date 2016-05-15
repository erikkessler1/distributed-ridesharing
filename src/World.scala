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
    print("-> ")
  }

  // Instructions for the command prompt
  private def printInstructions() = {
    print(Command.commands.mkString("\n"))
  }


  /* METHODS FOR COMMAND HANDLING */

  private def handleCommands() = {
    for (ln <- io.Source.stdin.getLines) {

      print(ANSI.up(1) + ANSI.right(ln.length + 3) + ANSI.delete + ANSI.left(ln.length + 3) + "-> ") 
      
      val command = ln.split(" ")(0)
      val args = ln.split(" ").toList.tail

      // Execute the command and print the result
      val paddedResult = Command.execute(this, command, args).padTo(35, ' ')
      print("\n   " + ANSI.style(ANSI.BOLD::Nil, paddedResult))
      print(ANSI.up(1) + ANSI.left(paddedResult.length))

    }
  }

  /* METHODS FOR PRINTING THE WORLD */
   
  /**
   * Prints the updated positions of the world.
   */ 
  def printWorld() = { 

    // Move to correct place for the line
    print(ANSI.move(4, 0))

    // Calculate the world line
    val (worldLine,ids) = makeWorldLine
    print(worldLine + makeGroundLine )
    print(idsToCols(ids))
    // Print the log

    print(ANSI.move(15 + Command.commands.size, 0) + ANSI.right(2))
  }

  /**
   * Prints the world line based on the list of peers.
   * Prints the focused peer as a purple block in the center of the line.
   * Prints all peers in the area as grey blocks if they are unknown to
   * the current focus and as cyan if they are know.
   */ 
  def makeWorldLine() = {

    val ids = new Array[Int](WIDTH)

    // Get all the peers in range of the current focus
    val right = focusedPeer.pos + (WIDTH / 2) - 1
    val left = focusedPeer.pos - (WIDTH / 2)
    val range = (left to right).map(n => (Util.worldSize + n) % Util.worldSize)

    val peersInRange = peers.filter(p => range.contains(p.pos))

    // Add the correct character and color for each place on the line
    var line = ""
    var i = 0
    for (p <- range) {

      line += (
      if (p == focusedPeer.pos) {
	ids(i) = focusedPeer.id
	ANSI.style(List(ANSI.PURPLE), "█") 
      } else if (peersInRange.exists(_.pos == p)) {
        if (focusedPeer.peerList.exists(_.pos == p)) {
	  ids(i) = focusedPeer.peerList.find(_.pos == p).get.id
	  ANSI.style(List(ANSI.CYAN), "█")
	} else {
	  ids(i) = peersInRange.find(_.pos == p).get.id
	  ANSI.style(List(ANSI.GRAY), "█")
	}
      } else {
	ids(i) = -1
	" "
      })

      i += 1
    }
    (line, ids)
  }

  def makeGroundLine() = {
    val modPos = focusedPeer.pos % 12

    val section = if (modPos > 6) {
      ("=" * (12 - modPos)) + ("+" * 6) + ("=" * (6 - (12 - modPos)))
    } else {
      ("+" * (6 - modPos)) + ("=" * 6) + ("+" * modPos)
    }

    section * 11
  }

  def idsToCols(ids: Array[Int]) = {
    val max = ids.max
    var ten = (max + 9) / 10

    var line = ""
    while (ten != 0) {
      for (i <- 0 until ids.length) {
	val id = ids(i)
	line += (id match {
	    case -1 => " "
	    case n  => val res = n / ten; ids(i) = n % ten; res
	})
      }
      ten /= 10
      line += "\n"
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
    focusedPeer.peerList = peers.filter(_.pos % 2 == 0)
    printWorld()
  }

  /**
   * Moves the world forward one step.
   */ 
  def step(steps: Int, delay: Int) = {
    for (i <- 1 to steps) {
      peers.foreach { _.step() }
      printWorld()
      if (steps > 1) Thread.sleep(delay)
    }
  }

}
