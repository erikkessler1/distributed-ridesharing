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
 abstract class Peer(id: Int, initialPos: Int) {
   // position of the peer in the world
   var pos: Int = initialPos

   // other nodes that this node is currently in communication with
   var peerList : List[Peer] = Nil

   var peerLog = List(s"Peer $id created.")

   // moves the peer through the world at each time step
   def step(): Int = {
     pos = wrap(pos)
     updatePeerList()
     pos
   }

   def logPosition(oldPos : Int) : Unit = {
     if (Util.verbose) peerLog = s"Moved from $oldPos to $pos" :: peerLog
   }

   // helper function to ensure that the peer wraps around when it reaches the end of the world
   private def wrap(x: Int): Int = { if (x < 0) Util.worldSize + x else x % Util.worldSize }

   private def updatePeerList() = {
     
   }
 }

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

   override def step(): Int = {

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
 }

 // Randomly moves around at various speeds or stands still
 class RandomMover(id: Int, initialPos: Int) extends Peer(id, initialPos) {

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
