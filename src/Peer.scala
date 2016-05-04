import scala.util._
/**
 * Class that represents a simulated peer in our peer-to-peer ride-sharing
 * system. The peer communicates with fellow peers via method calls and tells
 * them its position in the world as well as its closest neighbors.
 *
 * Erik Kessler and Kevin Persons
 */

object Peer {
  def main(args: Array[String]) = {
    var com = new Peer(10)
    args(0) match {
      case "Commuter" => com = new Commuter(20)
      case "Passenger" => com = new Passenger(20)
      case "Random" => com = new Random(20)
      case "Traveler" => com = new Traveler(20)
    }
    var i = 0
    while (i < 50) {
      println(com.step())
      i+=1
    }
  }
}

 class Peer(initialPos: Int) {
   var pos = initialPos
   def step(): Int = { return wrap(pos) }
   def wrap(x: Int): Int = { if (x < 0) 1000 + x else x % 1000 }
 }

 // type of peer that commutes a certain distance back and forth repeatedly
 class Commuter(initialPos: Int) extends Peer(initialPos) {
   val commuteLength = scala.util.Random.nextInt(46) + 15 //15-60
   val speed = scala.util.Random.nextInt(3) + 1 //1-3
   var progress = 0
   var direction = commuteLength % 2
   override def step(): Int = {
     if (progress < commuteLength) {
       if (direction == 0) pos += speed else pos -= speed //move
       progress = progress + speed //update progress
     }
     else {
       direction = (direction + 1) % 2 // reverse direction
       progress = 0 // reset progress
     }
     return wrap(pos)
   }
 }

 // type of peer that doesn't have a ride -- they are mostly standing still
 class Passenger(initialPos: Int) extends Peer(initialPos) {
   override def step(): Int = {
     val x = scala.util.Random.nextInt(10) //0-9
     x match {
       case 0 => pos += 1 //10% move right
       case 1 => pos -= 1 //10% move left
       case _ => //80% chance stand still
     }
     return wrap(pos)
   }
 }

 // type of peer that randomly moves around at various speeds or stands still
 class Random(initialPos: Int) extends Peer(initialPos) {
   override def step(): Int = {
     val r = scala.util.Random
     val x = r.nextInt(3) //0-2
     val speed = r.nextInt(3) + 1 //1-3
     x match {
       case 0 => pos = pos + speed
       case 1 => pos = pos - speed
       case 2 => // pos = pos, stand still
     }
     return wrap(pos)
   }
 }

 // type of peer that moves consistently in one direction, sometimes pausing
 class Traveler(initialPos: Int) extends Peer(initialPos) {
   val direction = scala.util.Random.nextInt(2) //0-1
   val speed = scala.util.Random.nextInt(3) + 1 //1-3
   override def step(): Int = {
     // small chance of not moving
     if (scala.util.Random.nextInt(15) == 0) {
       return wrap(pos)
     }
     if (direction == 0) pos += speed else pos -= speed
     return wrap(pos)
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
