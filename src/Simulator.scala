import scala.util.Random

/**
 * The program we will use to simulate and evaluate our P2P algorithms in
 * a simplified and controlled environment.
 *
 * We construct a World which holds a list of peers.
 * These peers exist
 *
 * Erik Kessler and Kevin Persons
 */
object Simulator {

  def main(args: Array[String]): Unit = {

    var numberOfPeers = Util.defaultPeers
    for (i <- 0 until args.length) {
      args(i) match {
        case "-v" => Util.verbose = true
        case "-r" => Util.randomStart = true
        case "-f" => Util.fullPeerLists = true
        case  _   => numberOfPeers = Util.toInt(args(i)).getOrElse(Util.defaultPeers)
      }
    }

    Util.numPeers = numberOfPeers

    // Randomly initialize a list of some number of peers, as specified by user input
    if (Util.randomStart) {
      val peers = initializeRandomly()
      World.peers = peers
      World.start()
    }
    // Initialize incrementally (more realistic)
    else {
      val firstPeer = createNewPeer(Util.numPeers)
      World.peers = List(firstPeer)
      World.start()
    }
  }

  def initializeRandomly() : List[Peer] = {

    var list : List[Peer] = Nil

    for (i <- 1 to Util.numPeers) {
      list = createNewPeer(i) :: list
    }

    for (peer <- list) {
      if (!Util.fullPeerLists) {
        peer.setPeerList(List.fill(10)(list(scala.util.Random.nextInt(Util.numPeers - 1) + 1)))
      }
      else {
        peer.setPeerList(list.filter(_ != peer))
      }
    }

    list
  }

  // each new peer to join the system begins with a single node peer list,
  // and the nodes join over time rather than all at once
  def initializeIncrementally() : List[Peer] = {

    var list = World.peers

    // create n-1 other new peers one at a time
    var peer = createNewPeer(Util.numPeers - list.length)
    list = peer :: list

    // randomly assign each new peer an existing peer to be on its start list
    peer.setPeerList(List.fill(1)(list(scala.util.Random.nextInt(list.length - 1) + 1)))

    list
  }

  def createNewPeer(i: Int) : Peer = {
    val n = Util.numPeers
    val random = scala.util.Random.nextInt(6) //0-5
    val initialPosition = scala.util.Random.nextInt(Util.worldSize + 1) //0-worldSize (using 1000)
    val newPeer = random match {
      case 0 => new Commuter(n - i, initialPosition)
      case 1 => new RandomMover(n - i, initialPosition)
      case 2 => new Traveler(n - i, initialPosition)
      // 3x as likely to be a passenger so that there are half drivers, half passengers
      case _ => new Passenger(n - i, initialPosition)
    }

    newPeer
  }
}
