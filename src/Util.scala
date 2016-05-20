/**
 * Collection of constants and helper functions for use
 * throughout the program.
 *
 * Erik Kessler and Kevin Persons
 */
object Util {

  // Number of postitions
  val worldSize = 1200

  val defaultPeers = 100

  var numPeers = 0

  var randomStart = false

  var fullPeerLists = false

  // Peers must be within disstance to match
  val matchDist = 10

  // Number of steps each ride lasts for
  val rideLength = 10

  // Number of spaces move before sending location update
  val updateDist = 10

  // Log extra information
  var verbose = false


  /**
   * Attempt to parse a string to an Int.
   * Returns Option.None if failed.
   */
  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }
}
