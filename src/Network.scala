/**
 * Models the network and maintains usage data.
 */
object Network {

  // Counts for important metrics
  var sentMessages = 0
  var rideRequests = 0
  var rideMatches = 0

  /**
   * Issues a ride request from the sender to the reciever.
   */
  def sendRequest(sender: Peer, reciever: Peer) : Boolean = {
    sentMessages += 2
    val matched = reciever.respondToRequest(sender)
    if (matched) rideMatches += 1
    return matched
  }

  def sendUpdate(sender: Peer, reciever: Peer): (List[FrozenPeer], Int) = {
    reciever.getUpdate(sender)
    (reciever.getPeerLocs.map(p => new FrozenPeer(p.id, p.pos, p.ts, p.peer)).filter(_.id != sender.id), reciever.pos)
  }

}