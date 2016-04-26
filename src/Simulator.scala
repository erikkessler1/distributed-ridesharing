import scala.util.Random

object Simulator {

  def main(args: Array[String]) = {
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
