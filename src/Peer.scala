import scala.util._
/**
 * Class that represents a simulated peer in our peer-to-peer ride-sharing
 * system. The peer communicates with fellow peers via method calls and tells
 * them its position in the world as well as its closest neighbors.
 *
 * Erik Kessler and Kevin Persons
 */

//Used for testing the different types of peer movement
object Peer {
  def main(args: Array[String]) = {
    var com : Peer = null
    args(0) match {
      case "Commuter" => com = new Commuter(0, 20)
      case "Passenger" => com = new Passenger(0, 20)
      case "RandomMover" => com = new RandomMover(0, 20)
      case "Traveler" => com = new Traveler(0, 20)
    }
    var i = 0
    while (i < 50) {
      println(com.step())
      i+=1
    }
  }
}

 // Base class for all types of Peers
abstract class Peer(val id: Int, initialPos: Int) {
  // position of the peer in the world
  var pos: Int = initialPos

  var lastReportedPos: Int = initialPos
  var lastReportTime: Int = 0

  // current ride-matched status of this peer
  protected var matched = false

  def respondToRequest(peer: Peer) : Boolean = {
    if (!matched && math.abs(peer.pos - pos) < Util.matchDist) {
      matched = true
      rideLength = Util.rideLength
      logGivingRide(peer.id, rideLength)
      true
    }
    else {
      false
    }
  }

  // other nodes that this node is currently in communication with
  protected var peerList : List[Peer] = Nil

  protected var peerLocs : List[FrozenPeer] = Nil

  protected var rideLength = 0

  def setPeerList(peers: List[Peer]) = {
   peerLocs = peers.map(p => new FrozenPeer(p.id, p.pos, World.time, p))
   peerList = peers
  }

  var peerLog = List(s"P$id created.    ")

  // moves the peer through the world at each time step
  def step(): Int = {
    rideLength match {
      case 0 =>
      case 1 => rideLength -= 1; matched = false; logRideEnd()
      case _ => rideLength -= 1
    }
    pos = wrap(pos)
    updatePeerList(pos)
    pos
  }

  def logPosition(oldPos : Int) : Unit = {
    if (Util.verbose) peerLog = s"Moved from $oldPos to $pos".padTo(45, ' ')::peerLog
  }

  def logGivingRide(passenger: Int, rideLength: Int) = {
    peerLog = s"Driving P$passenger, $rideLength steps.".padTo(45, ' ')::peerLog
  }

  def logGettingRide(driver: Int, rideLength: Int) = {
    peerLog = s"Riding with P$driver, $rideLength steps.".padTo(45, ' ')::peerLog
  }

  def logRideEnd() = {
    peerLog = s"Ride ended.".padTo(45, ' ')::peerLog
  }

  def logUpdatedPeerList() = {
    peerLog = "Sent updated location.".padTo(45, ' ')::peerLog
  }

  def logRecievedUpdate(sender: Peer) = {
    peerLog = s"Got update from ${sender.id}".padTo(45, ' ')::peerLog
  }

  def getPeerList() = peerLocs.map(_.peer)

  def getPeerLocs() = peerLocs

  // helper function to ensure that the peer wraps around when it reaches the end of the world
  private def wrap(x: Int): Int = { if (x < 0) Util.worldSize + x else x % Util.worldSize }

  private def updatePeerList(newPos: Int): Unit = {
    if (math.abs(lastReportedPos - newPos) < Util.updateDist && World.time - lastReportTime < 20) return

    logUpdatedPeerList()

    var newPeerList: List[FrozenPeer] = Nil

    for(peer <- peerLocs) {
      val (newPeers, newPos) = Util.sendUpdate(this, peer.peer)

      // Update what we know about this peer
      peer.pos = newPos
      peer.ts = World.time

      // Store the new peer information
      newPeerList = newPeers ::: newPeerList
    }

    filterNewPeers(newPeerList)

    lastReportedPos = pos
    lastReportTime = World.time
  }

  private def filterNewPeers(newPeers: List[FrozenPeer]) = {
    for (newPeer <- newPeers) {
      peerLocs.find(_.id == newPeer.id) match {
	case Some(p) => if (newPeer.ts > p.ts) { p.ts = newPeer.ts; p.pos = newPeer.pos }
	case None    => peerLocs = newPeer::peerLocs
      }
    }

    peerLocs = peerLocs.sortBy(p => math.abs(p.pos - pos))
    peerLocs = peerLocs.take(10)
  }

  def getUpdate(sender: Peer) = {
    logRecievedUpdate(sender)
    filterNewPeers(List(new FrozenPeer(sender.id, sender.pos, World.time, sender)))
  }

}

/**
 * Holds information about a peer in a frozen state.
 * This models what a node would hold about each peer it knows about.
 * 
 * id - id of the peer
 * pos - know pos of the peer
 * ts - timestamp of the known pos of the peer
 * peer - the actual peer object - so this would be like the ip address that
 *        would allow us to actually communicate with the peer.
 */ 
class FrozenPeer(val id: Int, var pos: Int, var ts: Int, val peer: Peer)

/* --TYPES OF PEERS-- */

// Commutes a certain distance back and forth repeatedly
class Commuter(id: Int, initialPos: Int) extends Peer(id, initialPos) {

  val commuteLength = scala.util.Random.nextInt(46) + 15 //15-60
  val speed = scala.util.Random.nextInt(3) + 1 //1-3
  var progress = 0
  var direction = commuteLength % 2

  override def step(): Int = {

    val oldPos = pos
    if (progress < commuteLength) {
      if (direction == 0) pos += speed else pos -= speed //move
      progress = progress + speed //update progress
     }
     else {
       direction = (direction + 1) % 2 // reverse direction
       progress = 0 // reset progress
     }

     logPosition(oldPos)
     return super.step
  }
}

// Doesn't have a ride -- they are mostly standing still
class Passenger(id: Int, initialPos: Int) extends Peer(id, initialPos) {

  var status = 0

  override def step(): Int = {

    if (!matched && status == 0 && scala.util.Random.nextInt(9) == 0) {
      status = 5
      sendRideRequest(true)
    }
    if (status > 0) {
      status -= 1
      sendRideRequest(false)
    }

    val oldPos = pos
    val x = scala.util.Random.nextInt(10) //0-9
    x match {
      case 0 => pos += 1 //10% move right
      case 1 => pos -= 1 //10% move left
      case _ => //80% chance stand still
    }

    logPosition(oldPos)
    return super.step
  }

  /**
   * The node asks all peers it knows if they can give a ride.
   * If newRequest is true, we increment our request counter.
   * It is false when a request spans multiple steps.
   */ 
  private def sendRideRequest(newRequest :Boolean) : Unit = {
    
    // Increment request count if it is a new request
    if (newRequest) Util.rideRequests += 1
    
    // Try to match with every peer in the peer list
    for(peer <- peerLocs.sortBy(p => math.abs(p.pos - pos))) {
      if (Util.sendRequest(this, peer.peer)) {
        matched = true
        rideLength = Util.rideLength
        logGettingRide(id, rideLength)
        return
      }
    }
  }

  // They don't ever give rides.
  override def respondToRequest(peer: Peer) : Boolean = false

}

// Randomly moves around at various speeds or stands still
class RandomMover(id: Int, initialPos: Int) extends Peer(id, initialPos) {

  var offering = false

  override def step(): Int = {

    val oldPos = pos
    val r = scala.util.Random
    val x = r.nextInt(3) //0-2
    val speed = r.nextInt(3) + 1 //1-3
    x match {
      case 0 => pos = pos + speed
      case 1 => pos = pos - speed
      case 2 => // stand still
    }

    logPosition(oldPos)
    return super.step
  }
}

// Moves consistently in one direction, sometimes pausing
class Traveler(id: Int, initialPos: Int) extends Peer(id, initialPos) {

  val direction = scala.util.Random.nextInt(2) //0-1
  val speed = scala.util.Random.nextInt(3) + 1 //1-3

  override def step(): Int = {

    val oldPos = pos
    // small chance of not moving
    if (scala.util.Random.nextInt(15) == 0) {
      return super.step
    }
    if (direction == 0) pos += speed else pos -= speed

    logPosition(oldPos)
    return super.step
  }
}

 /*
 types of peer movement:

 long back and forth (commuters)
-------------------------->
<--------------------------
-------------------------->

people without a ride (passengers)
->
 .
 .
<-

random movement (random)
  ---->
    <--
<---
.
---------->

near continuous movement in one direction (traveler)
-------------------->
                    .
                    .
                    --------------------->
                                         .
                                         ---------------------------->
*/
