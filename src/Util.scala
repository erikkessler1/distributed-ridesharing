object Util {


  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }

  def sendRequest(sender: Peer, reciever: Peer) : Boolean = {
    sentMessages += 2
    val matched = reciever.respondToRequest(sender)
    if (matched) matches += 1
    return matched
  }

  def sendUpdate(sender: Peer, reciever: Peer): (List[FrozenPeer], Int) = {
    reciever.getUpdate(sender)
    (reciever.getPeerLocs.map(p => new FrozenPeer(p.id, p.pos, p.ts, p.peer)).filter(_.id != sender.id), reciever.pos)
  }

  val worldSize = 1200

  val defaultPeers = 100

  var numPeers = 0

  var verbose = false

  var randomStart = false

  var fullPeerLists = false

  val matchDist = 10

  val rideLength = 10

  val updateDist = 10

  // counts for important metrics
  var sentMessages = 0
  var rideRequests = 0
  var matches = 0

}
