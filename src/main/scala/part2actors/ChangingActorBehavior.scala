package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ChangingActorBehavior.Mom.MomStart

object ChangingActorBehavior extends App {

  object FussyKid {
    case object Accept
    case object Reject
    val HAPPY = "happy"
    val SAD = "sad"
  }

  class FussyKid extends Actor {
    import FussyKid._
    import Mom._
    //internal state of Kid
    var state = HAPPY
    val name = "[Kid actor]"

    override def receive = {
      case Food(VEGETABLES) => state = SAD
      case Food(CHOCOLATE) => state = HAPPY
      case Ask(_) => {
        if(state == HAPPY)
          sender ! Accept
        else
          sender ! Reject
      }
    }
  }

  class StateLessFussyKid extends Actor {
    import FussyKid._
    import Mom._
    override def receive: Receive = happyReceive

    def happyReceive : Receive = {
      case Food(VEGETABLES) => context.become(sadReceive, false) //change the receive handler to sadReceive
      case Food(CHOCOLATE) => context.become(happyReceive, false)
      case Ask(_) => sender ! Accept
    }

    def sadReceive : Receive = {
      case Food(VEGETABLES) => context.become(sadReceive, false)
      case Food(CHOCOLATE) => context.unbecome() //change the receive handler to happyReceive
      case Ask(_) => sender ! Reject
    }
  }

  object Mom {
    case class MomStart(ref: ActorRef)
    case class Food(food : String)
    case class Ask(message : String) //wanna play
    val VEGETABLES = "veggies"
    val CHOCOLATE = "chocolate"
  }

  class Mom extends Actor {
    import Mom._
    import FussyKid._
    val name = "[Mom actor]"

    override def receive = {
      case MomStart(kid) => {
        kid ! Food(VEGETABLES)
        kid ! Food(VEGETABLES)
        kid ! Ask("Do you want to play")
        kid ! Food(CHOCOLATE)
        kid ! Ask("Do you want to play")
        kid ! Food(CHOCOLATE)
        kid ! Ask("Do you want to play")
      }
      case Reject => println("My kid is sad, but atleast he is healthy")
      case Accept => println("Yayy!! My kid is happy")

    }
  }

  val system = ActorSystem("ChangingActorBehaviourDemo")
  val fussyKid = system.actorOf(Props[FussyKid], "fussyKid")
  val mom = system.actorOf(Props[Mom], "mom")
  val stateLessFussyKid = system.actorOf(Props[StateLessFussyKid], "statelessFussyKid")
  //mom ! MomStart(fussyKid)
  /**
   * context.become(messageHandler, false)
   *
   * will push the messageHandler so that it becomes on top
   *
   * In case of starting with happyReceive
   * we received two messages
   * Veg
   * Veg
   * The stack would be:
   * sadReceive
   * sadReceive
   * happyReceive
   *
   * Next if we receive: Chocolate
   *
   * The stack would be:
   *
   * sadReceive
   * happyReceive
   *
   * Next of we receive another: Chocolate
   *
   * The stack would be empty leaving with the original messageHandler:
   *
   * happyReceive
   */
  mom ! MomStart(stateLessFussyKid)


  /**
   * Exercise: Recreate Counter Actor without mutable State using context.become
   */
  object CounterActor {
    case object Start
    case object Increment
    case object Decrement
    case object PrintCounter
  }

  class CounterActor extends Actor {
    import CounterActor._

    val name = "[Counter Actor]"
    override def receive = countReceive(0)

    def countReceive(counter : Int) : Receive = {
      case Increment => context.become(countReceive(counter + 1))
      case Decrement => context.become(countReceive(counter - 1))
      case PrintCounter => println(s"$name counter : $counter")
    }

    /*def decrementReceive(counter : Int) : Receive = {
      case Increment => context.become(incrementReceive(counter + 1))
      case Decrement => context.become(decrementReceive(counter - 1), false)
      case PrintCounter => println(s"$name counter : $counter")
    }

    def incrementReceive(counter: Int) : Receive = {
      case Increment => context.become(incrementReceive(counter + 1), false)
      case Decrement => context.become(decrementReceive(counter - 1 ), false)
      case PrintCounter => println(s"$name counter : $counter")
    }*/
  }

  val counterActor = system.actorOf(Props[CounterActor], "counterActor")
  import CounterActor._
  (1 to 10).foreach(_ => counterActor ! Increment)
  counterActor ! PrintCounter
  (1 to 8).foreach(_ => counterActor ! Decrement)
  counterActor ! PrintCounter

}
