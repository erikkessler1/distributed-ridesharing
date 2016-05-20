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
    // Increment by two because we send data both ways
    sentMessages += 2

    // See how the reciever responds and update rideMatches appropriately
    val matched = reciever.respondToRequest(sender)
    if (matched) rideMatches += 1
    
    return matched
  }

  /**
   * Sends an update about location.
   * Sender sends its updated positions and the receiver responds with its peer list.
   */
  def sendUpdate(sender: Peer, reciever: Peer): (List[FrozenPeer], Int) = {
    // Communication is two-way
    sentMessages += 2

    // Send the update to the reciever
    reciever.getUpdate(sender)

    // Send the reciever's peer list back to the initial sender
    (reciever.getFrozenPeerList.map(p => new FrozenPeer(p.id, p.pos, p.ts, p.peer)).filterNot(_.id == sender.id), reciever.pos)
  }

}