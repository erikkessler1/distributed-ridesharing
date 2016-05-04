import scala.util.Random

/**
 * The program we will use to simulate and evaluate our P2P algorithms in
 * a simplified and controlled environment.
 *
 * We have only just begun working on this, so it is very rough but it
 * shows the general idea of what we are imagining: having the
 * ability to add/remove virtual machines to our network and have
 * them take actions related to ride-sharing.Then having the ability
 * to view both the global state of the network and the state of each
 * machine to determine how information is flowing through the network.
 *
 * Erik Kessler and Kevin Persons
 */
object Simulator {

  def main(args: Array[String]): Unit = {

    val numberOfPeers = if (args.length > 0) Util.toInt(args(0)).getOrElse(100) else 100

    // Initialize a list of some number of peers, as specified by user input
    val peers = initializePeers(numberOfPeers)
    println(peers)

    val world = new World(100, peers)
    world.start()
  }

  def initializePeers(n: Int) : List[Peer] = {
    var list : List[Peer] = Nil
    for (i <- 1 to n) {
      val random = scala.util.Random.nextInt(4) //0-3
      val initialPosition = scala.util.Random.nextInt(1001) //0-1000
      list = random match {
        case 0 => new Commuter(initialPosition) :: list
        case 1 => new Passenger(initialPosition) :: list
        case 2 => new RandomMover(initialPosition) :: list
        case 3 => new Traveler(initialPosition) :: list
      }
    }
    return list
  }
}
