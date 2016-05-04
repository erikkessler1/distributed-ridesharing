/**
 * Class that holds the state of the world for the simulator
 * and provides methods to display itself on the terminal.
 *
 * First call printInitialWorld to setup the terminal, then use printWorld
 * to display the updated world.
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
   * Sets the focused peer to be the peer at index i.
   */ 
  def setFocusedPeer(i: Int) = peers(i)

  /**
   * Prints the template for the world.
   */
  def printInitialWorld() = { 
    print(ANSI.clear)
    createWorldLine()
    createDividingLine()
    createLogArea()
    createCommandPrompt()
  }

  private def createWorldLine() = {
    print(ANSI.move(5,0))
    print("_" * WIDTH)
  }

  private def createDividingLine() = {
    for (r <- 12 until HEIGHT) {
      print(ANSI.move(r, WIDTH/2) + "|")
    }
  }

  private def createLogArea() = {
    print(ANSI.move(12, WIDTH/2 + 1) + 
	  ANSI.style(List(ANSI.BOLD, ANSI.UNDERLINE), "Log for Current Peer:"))
  }

  private def createCommandPrompt() = {
    print(ANSI.move(12, 0) + 
	  ANSI.style(List(ANSI.BOLD, ANSI.UNDERLINE), "Command Prompt:"))
    print(ANSI.move(14, 0))
    printInstructions()
    print(ANSI.down(1))
    print("\n-> ")
  }

  private def printInstructions() = {
    print("s    : Step simulation\n" + 
	  "f [n]: Set focus on peer n")
  }

  /**
   * Prints the updated positions of the world.
   */ 
  def printWorld() = { }

}


class Peer {

}
