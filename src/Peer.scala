import scala.util._
/**
 * Class that represents a simulated peer in our peer-to-peer ride-sharing
 * system. The peer communicates with fellow peers via method calls and tells
 * them its position in the world as well as its closest neighbors.
 *
 * Erik Kessler and Kevin Persons
 */


/**
 * Base class for all types of Peers.
 */
abstract class Peer(val id: Int, initialPos: Int) {

  // Position of the peer in the world
  var pos: Int = initialPos

  // Log of activity performed by the peers
  var peerLog = List(s"P$id created.    ")

  // Record information about when an update was last sent
  protected var lastReportedPos: Int = initialPos
  protected var lastReportTime: Int = 0

  // Other peers this node currently knows about
  protected var peerLocs : List[FrozenPeer] = Nil

  // Distance left in the ride
  protected var rideLength = 0
  
  // Current ride-matched status of this peer
  protected var matched = false

  /**
   * Convert the peer list into a list of FrozenPeers so we
   * can record the postion we know about.
   */
  def setPeerList(peers: List[Peer]) = {
   peerLocs = peers.map(p => new FrozenPeer(p.id, p.pos, World.time, p))
  }

  /**
   * Moves the peer through the world at each time step.
   * Calls abstract method stepAction to determine the next position.
   */
  def step() = {

    // Adjust the rideLength counter
    rideLength match {
      case 0 => // Ride not ongoing, do nothing
      case 1 => rideLength -= 1; matched = false; logRideEnd() // Ride ending
      case _ => rideLength -= 1 // Decrement by 1
    }

    // Get the new position
    val (oldPos, newPos) = stepAction(pos)
    pos = Util.wrap(newPos)
    logPosition(oldPos)

    // Update the peer list
    updatePeerList()
  }

  /**
   * Abstract methods that defines how to move.
   */
  def stepAction(currentPos: Int): (Int, Int)

  private def updatePeerList(): Unit = {
    if (math.abs(lastReportedPos - pos) < Util.updateDist && World.time - lastReportTime < 20) return

    logUpdatedPeerList()

    var newPeerList: List[FrozenPeer] = Nil

    for(peer <- peerLocs) {
      val (newPeers, newPos) = Network.sendUpdate(this, peer.peer)

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

  def getPeerList() = peerLocs.map(_.peer)

  def getFrozenPeerList() = peerLocs

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

  def getUpdate(sender: Peer) = {
    logRecievedUpdate(sender)
    filterNewPeers(List(new FrozenPeer(sender.id, sender.pos, World.time, sender)))
  }


  /* ----- LOGGING METHODS ----- */
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



/* ------- TYPES OF PEERS ------- */



// Commutes a certain distance back and forth repeatedly
class Commuter(id: Int, initialPos: Int) extends Peer(id, initialPos) {

  // Set random length and speed
  val commuteLength = Random.nextInt(46) + 15 //15-60
  val speed = Random.nextInt(3) + 1 //1-3
  var direction = commuteLength % 2

  // Progress along commuteLength
  var progress = 0

  override def stepAction(currentPos: Int) = {
  
    val newPos = if (progress < commuteLength) {
      // Update the progress
      progress = progress + speed

      // Calculate the new position
      if (direction == 0) currentPos + speed else currentPos - speed 
    } else {
      direction = (direction + 1) % 2 // reverse direction
      progress = 0 // reset progress
      currentPos // position is the same
    }

    (currentPos, newPos)
  }
}

// Doesn't have a ride -- they are mostly standing still
class Passenger(id: Int, initialPos: Int) extends Peer(id, initialPos) {

  // Stores how many steps we have been requesting for
  // Each request lasts five steps.
  private var requestStatus = 0

  override def stepAction(currentPos: Int) = {

    // Randomly request a ride 10% of the time
    if (!matched && requestStatus == 0 && Random.nextInt(9) == 0) {
      requestStatus = 5
      sendRideRequest(true)
    } else if (requestStatus > 0) {
      requestStatus -= 1
      sendRideRequest(false)
    }

    // Calculate the new position
    val newPos = Random.nextInt(10) match {
      case 0 => currentPos + 1 //10% move right
      case 1 => currentPos - 1 //10% move left
      case _ => currentPos //80% chance stand still
    }

    (currentPos, newPos)
  }

  /**
   * The node asks all peers it knows if they can give a ride.
   * If newRequest is true, we increment our request counter.
   * It is false when a request spans multiple steps.
   */ 
  private def sendRideRequest(newRequest :Boolean) : Unit = {
    
    // Increment request count if it is a new request
    if (newRequest) Network.rideRequests += 1
    
    // Try to match with every peer in the peer list
    for(peer <- peerLocs.sortBy(p => math.abs(p.pos - pos))) {
      if (Network.sendRequest(this, peer.peer)) {
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

  override def stepAction(currentPos: Int) = {

    val speed = Random.nextInt(3) + 1 //1-3
    
    val newPos = Random.nextInt(3) match {
      case 0 => currentPos + speed
      case 1 => currentPos - speed
      case 2 => currentPos
    }

    (currentPos, newPos)
  }
}

// Moves consistently in one direction, sometimes pausing
class Traveler(id: Int, initialPos: Int) extends Peer(id, initialPos) {

  val direction = Random.nextInt(2) //0-1
  val speed = Random.nextInt(3) + 1 //1-3

  override def stepAction(currentPos: Int) = {
  
    val newPos = if (Random.nextInt(15) == 0) {
      currentPos // Small chance of not moving
    } else {
      if (direction == 0) currentPos + speed else currentPos - speed
    }
    
    (currentPos, newPos)
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
