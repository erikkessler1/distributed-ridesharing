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

  val worldSize = 1200

  val defaultPeers = 100

  var verbose = false

  var randomStart = false

  var fullPeerLists = false

  val matchDist = 10

  val rideLength = 10


  // counts for important metrics
  var sentMessages = 0
  var rideRequests = 0
  var matches = 0

}
