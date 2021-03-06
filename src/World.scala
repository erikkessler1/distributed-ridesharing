/**
 * Class that holds the state of the world for the simulator
 * and provides methods to display itself on the terminal.
 *
 * We assume a linear world that wraps around.
 *
 * Erik Kessler and Kevin Persons
 */
object World {

  // Peer objects in the world
  var peers: List[Peer] = Nil

  // Universal clock - increments with each step
  var time = 0

  // Peer to center the world around
  private var focusedPeer: Peer = null


  // Height and width of the terminal window
  private val WIDTH = 132
  private val HEIGHT = 43

  /**
   * Setup the world and begin accepting commands to control the world.
   */
  def start() = {
    focusedPeer = peers(0)
    time = 0
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
    createPeerListArea()
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
  private def createLogArea() : Unit = {
    print(ANSI.move(12, WIDTH/2 + 1) +
	  ANSI.style(List(ANSI.BOLD, ANSI.UNDERLINE), "Log for Current Peer:"))
  }

  // Area for log entries for the current node.
  private def createPeerListArea() : Unit = {
    print(ANSI.move(12, 3*WIDTH/4 + 1) +
    ANSI.style(List(ANSI.BOLD, ANSI.UNDERLINE), "Current Peer's List:"))
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
      val paddedResult = Command.execute(command, args).padTo(35, ' ')
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
    printLog()
    // Print the focused node's list of peers
    printPeerList()

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
        if (focusedPeer.getPeerList.exists(_.pos == p)) {
	  ids(i) = focusedPeer.getPeerList.find(_.pos == p).get.id
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

  // Prints the gound line as alternating +++++++======
  def makeGroundLine() = {
    val modPos = focusedPeer.pos % 12

    val section = if (modPos > 6) {
      ("=" * (12 - modPos)) + ("+" * 6) + ("=" * (6 - (12 - modPos)))
    } else {
      ("+" * (6 - modPos)) + ("=" * 6) + ("+" * modPos)
    }

    section * 11
  }


  // Prints the log for the focused peer
  def printLog() : Unit = {
    var h = 12
    for (msg <- focusedPeer.peerLog) {
      h += 1
      if (h >= HEIGHT) return
      print(ANSI.move(h, WIDTH/2 + 1) + msg)
    }
  }

  // Prints the peer list of the focused peer
  def printPeerList() : Unit = {
    var h = 12
    for (peer <- focusedPeer.getFrozenPeerList) {
      h += 1
      if (h >= HEIGHT) return
      print(ANSI.move(h, 3*WIDTH/4 + 1) + s"P${peer.id}, Location ${peer.pos}    ")
    }
  }

  /**
   * Convert array of ids into a string that can be printed to show the ids of
   * the peers.
   */
  def idsToCols(ids: Array[Int]) = {
    // Find the max id
    val max = Util.numPeers

    // Round max down to nearest power of 10
    var ten = (max + 9) / 10

    var line = ""
    while (ten != 0) {
      for ((id,i) <- ids.zipWithIndex) {

	// Add the digit. Take the mod
	line += (id match {
	    case -1 => " "
	    case n  => val res = n / ten; ids(i) = n % ten; res
	})
      }

      // Move to a lower power of 10
      ten /= 10
      line += "\n"
    }

    line // Return the string
  }

  /* METHODS FOR MANIPULATING THE WORLD */

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
  def step(steps: Int, delay: Int) = {
    for (i <- 1 to steps) {
      time += 1

      // we're still in startup phase of a realistic simulation: add new peers between steps
      if ((peers.length != Util.numPeers) && !Util.randomStart) {
        peers = Simulator.initializeIncrementally()
      }

      peers.foreach { _.step() }
      printWorld()
      if (steps > 1) Thread.sleep(delay)
    }
  }

}
