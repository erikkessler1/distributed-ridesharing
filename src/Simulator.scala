import scala.util.Random

/**
 * The program we will use to simulate and evaluate our P2P algorithms in
 * a simplified and controlled environment.
 *
 * See the README for usage information.
 *
 * Erik Kessler and Kevin Persons
 */
object Simulator {

  /**
   * Interpret the command line arguments, construct the peer list,
   * and start the world.
   */
  def main(args: Array[String]): Unit = {

    Util.numPeers = Util.defaultPeers

    // Interpret the arguments
    for (i <- 0 until args.length) {
      args(i) match {
        case "-v" => Util.verbose = true
        case "-r" => Util.randomStart = true
        case "-f" => Util.fullPeerLists = true
        case  _   => Util.numPeers = Util.toInt(args(i)).getOrElse(Util.defaultPeers)
      }
    }


    // Create the list of peers in the way specified by the user
    if (Util.randomStart) {
      // Initialize peers randomly
      val peers = initializeRandomly()
      World.peers = peers
    } else {
      // Initialize peers incrementally
      val firstPeer = createNewPeer(0)
      World.peers = List(firstPeer)
    }

    // Start the world
    World.start
  }

  /**
   * Generates a list of Util.numPeers peers at random positions
   * in the world.
   */
  def initializeRandomly() : List[Peer] = {

    // Create the list of
    val list = (0 until Util.numPeers).map(i => createNewPeer(i)).toList

    // Initialize the peer list, or set of peers a node knows about, for each node
    for (peer <- list) {
      if (Util.fullPeerLists) {

         // Set the peer list to be all except the peer itself
      	 peer.setPeerList(list.filterNot(_ == peer))
      } else {

        // Generate a list of ten random peers
        val randomList = List.fill(10)(list(Random.nextInt(Util.numPeers - 1)))

        // Set the random list as the peer list
        peer.setPeerList(randomList.distinct.filterNot(_ == peer))
      }
    }

    list
  }

  // each new peer to join the system begins with a single node peer list,
  // and the nodes join over time rather than all at once
  /**
   * Creates the list of peers by adding one at a time and
   * each new peer to join is given a single node from the
   * existing system.
   *
   * This buildup of the network over time with a single point
   * of entry is a more realistic model of reality.
   */
  def initializeIncrementally() : List[Peer] = {

    var list = World.peers

    // Create new peers one at a time
    var peer = createNewPeer(list.length)

    // Randomly assign each new peer an existing peer to be on its start list
    val peerOfEntry = List.fill(1)(list(if (list.length == 1) 0 else Random.nextInt(list.length - 1)))
    peer.setPeerList(peerOfEntry)

    list:::(peer::Nil) // Return the list with the new peer
  }

  /**
   * Create a new peer of a random type, at a random position.
   */
  def createNewPeer(id: Int) : Peer = {

    val initialPosition = Random.nextInt(Util.worldSize + 1)

    // Return a new peer
    Random.nextInt(6) match {
      case 0 => new Commuter(id, initialPosition)
      case 1 => new RandomMover(id, initialPosition)
      case 2 => new Traveler(id, initialPosition)

      // 3x as likely to be a passenger so that there are half drivers, half passengers
      case _ => new Passenger(id, initialPosition)
    }
  }

  def findNetworkPartitions() : Int = {
    var partitions : Int = 0
    // maintain a list of peers with boolean "visited" flags all initially false
    var traversableList = World.peers.map(p => new TraversablePeer(p,false))
    for (peer <- traversableList) {
      // peer has not been visited yet
      if (peer.visited != true) {
        peer.visited = true // visit it
        partitions += 1
        traversePeerList(peer, traversableList)
      }
    }
    partitions
  }

  def traversePeerList(peer : TraversablePeer, traversableList : List[TraversablePeer]) : Unit = {
    for (n <- peer.peer.getPeerList()) {
      var neighbor = (traversableList find {t : TraversablePeer => t.peer.id == n.id}).get
      if (neighbor.visited != true) {
        neighbor.visited = true
        traversePeerList(neighbor, traversableList)
      }
      for (p <- traversableList) {
        if (!p.visited && p.peer.getPeerList().exists(_.id == peer.peer.id)) {
          p.visited = true
          traversePeerList(p, traversableList)
        }
      }
    }
  }
}

class TraversablePeer(val peer: Peer, var visited: Boolean)
