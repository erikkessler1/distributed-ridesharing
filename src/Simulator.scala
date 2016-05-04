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

    val world = new World(100, Nil)
    world.start()

    return

    print("\u001b[42mBlue text with yellow background\u001b[0m\u001b[2J\u001b[0;0HHello World!\u001b[5;0HEnter Stuff: blah blah\u001b[5;14H\u001b[K")

    for (ln <- io.Source.stdin.getLines) {
      val command = ln.split(" ")(0)
      val arguments = ln.split(" ").toList.tail

      val result = command match {
	case "connect"    => connect(arguments.headOption)
	case "disconnect" => "Disconnecting"
	case "peers"      => printPeers(arguments.headOption)
	case default      => "Invalid Command"
      }

      println(result)
    }
  }

  private def connect(name: Option[String]) = {
    
    val machine = name match {
      case Some(n) => MachinePool.getMachine(n)
      case None    => MachinePool.newMachine()
    }

    val peer = machine.connect()

    name match {
      case Some(n) => s"connected existing $machine to $peer"
      case None    => s"created new $machine to $peer"
    }
  }

  private def printPeers(name: Option[String]) =
    name match {
      case Some(n) => MachinePool.getMachine(n).printPeers
      case None    => "TODO"
    }
}

/**
 * This allows us view the global state of the system.
 */ 
object MachinePool {

  var machineMap = Map[String, Machine]()
  var count = 0

  def newMachine() = {
    val newMachine = new Machine(count.toString)
    machineMap += count.toString -> newMachine
    count += 1
    newMachine
  }

  def getMachine(name: String) = {
    machineMap(name)
  }

  def centralMachine(machine: Machine) = {
    machineMap.getOrElse("0", machine)
  }

}

/**
 * We will model machines using objects and give them the functionality
 * that we would want for a ride-sharing application.
 */ 
class Machine(name: String) {

  var peers = List[Machine]()

  var connected = false

  def connect(): Machine = {
    connected = true
    peers match {
      case m::ms => {
	val (result, newPeer) = m.establishConnection(this)
	if (result) { peers = newPeer::peers; m } else { peers = ms; connect() }
      }
      case nil   => MachinePool.centralMachine(this)
    }
  }

  def establishConnection(peer: Machine) = {
    val randPeer = peers(Random.nextInt(peers.size))
    if (connected) peers = peer::peers
    println(peers)
    (connected, randPeer)
  }

  def printPeers() = peers.mkString(" ")

  override def toString() = name

}
