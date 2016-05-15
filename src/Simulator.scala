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

    Util.verbose = if (args.length > 0 && args(0) == "-v") true else false

    val numberOfPeers = if (args.length > 0) Util.toInt(args(0)).getOrElse(100) else 100

    // Initialize a list of some number of peers, as specified by user input
    val peers = initializePeers(numberOfPeers)

    val world = new World(peers)
    world.start()
  }

  def initializePeers(n: Int) : List[Peer] = {
    var list : List[Peer] = Nil
    for (i <- 1 until n) {
      val random = scala.util.Random.nextInt(6) //0-5
      val initialPosition = scala.util.Random.nextInt(Util.worldSize + 1) //0-worldSize (using 1000)
      list = random match {
        case 0 => new Commuter(n - i, initialPosition) :: list
        case 1 => new RandomMover(n - i, initialPosition) :: list
        case 2 => new Traveler(n - i, initialPosition) :: list
        // 3x as likely to be a passenger so that there are half drivers, half passengers
        case _ => new Passenger(n - i, initialPosition) :: list
      }
    }
    list = new Passenger(n-1,20) :: list

    for (peer <- list) {
      peer.setPeerList(List.fill(10)(list(scala.util.Random.nextInt(n - 1) + 1)))
    }

    return list
  }
}
