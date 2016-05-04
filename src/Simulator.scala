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

    // Initialize list of peers
    val peers = Nil

    val world = new World(100, peers)
    world.start()

  }
}
