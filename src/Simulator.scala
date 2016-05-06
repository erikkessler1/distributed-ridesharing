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

    val numberOfPeers = if (args.length > 0) Util.toInt(args(0)).getOrElse(100) else 100

    // Initialize a list of some number of peers, as specified by user input
    val peers = initializePeers(numberOfPeers)
    println(peers)

    val world = new World(peers)
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
