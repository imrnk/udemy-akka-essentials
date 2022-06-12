package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorCapabilities extends App {

  val actorSystem = ActorSystem("ActorCapabilities")

  class SimpleActor extends Actor {
    val name = "[Simple Actor]"
    override def receive = {
      case "Hi" => sender ! "Hello There"
      case message : String => println(s"$self I have received $message")
      case number  : Int => println(s"$name I have received a number: $number")
      case SpecialMessage(special) => println(s"$name I have received special message: ${special}")
      case SendMessageToYourself(msgtoSelf) => self ! msgtoSelf //sending the message to itself
        //will go to String case match pattern
      case SayHiTo(ref) => ref ! "Hi" //alice is being passed as the sender
      case WirelessPhoneMessage(content, ref) => ref forward (content + "s") //keep the original sender of message
    }
  }

  val simpleActor = actorSystem.actorOf(Props[SimpleActor], "simpleActor")

  //1  Messages can be of any type
  // a) Messages must be IMMUTABLE
  // b) Messages must be SERIALIZABLE ( In practice use case classes and case objects)

  simpleActor ! "Hello Actor"
  //simpleActor ! 42

  case class SpecialMessage(contents: String)
  //simpleActor ! SpecialMessage("special contents")

  //2  actors have information about their context and about themselves
  //context.self === `this` in OOP

  case class SendMessageToYourself(content: String)

  simpleActor ! SendMessageToYourself("I am an actor")

  //3 - actors can REPLY to messages

  val alice  = actorSystem.actorOf(Props[SimpleActor], "Alice")
  val bob  = actorSystem.actorOf(Props[SimpleActor], "Bob")

  case class SayHiTo(actorRef: ActorRef)
  alice ! SayHiTo(bob)

  //[INFO] [06/04/2022 18:26:13.710] [ActorCapabilities-akka.actor.default-dispatcher-5]
  // [akka://ActorCapabilities/deadLetters] Message [java.lang.String] from
  // Actor[akka://ActorCapabilities/user/Alice#-1508221039] to
  // Actor[akka://ActorCapabilities/deadLetters] was not delivered.
  // [1] dead letters encountered. If this is not an expected behavior,
  // then [Actor[akka://ActorCapabilities/deadLetters]] may have terminated unexpectedly,
  // This logging can be turned off or adjusted with configuration settings
  // 'akka.log-dead-letters' and 'akka.log-dead-letters-during-shutdown'.
  //4 - dead letter
  // alice ! "Hi" //reply to "me"

  //5 Forwarding messages
  case class WirelessPhoneMessage(content : String, actorRef: ActorRef)

  alice ! WirelessPhoneMessage("Hi", bob)

  /**
   * Exercise
   * 1. a Counter Actor
   *    - Increment
   *    - Decrement
   *    - Print
   *
   *  2. a BankAccount Actor
   *   receives:
   *   - Deposit message amount
   *   - Withdraw amount message
   *   - Statement
   *   responds:
   *   - Success/Failure
   *
   *   interact with some other actor
   *
   */

  /**
   * Exercise 1
   */
 //Best practice - Domain of the Counter - put the messages inside companion object
  object CounterActor {
    case object Increment
    case object Decrement
    case object PrintCounter
  }

  class CounterActor extends Actor {
    import CounterActor._
    val name = "[Counter Actor]"
    var counter = 0
    override def receive = {
      case Increment => counter += 1
      case Decrement => counter -= 1
      case PrintCounter => println(s"$name counter is : $counter")
    }
  }

  val counterActor = actorSystem.actorOf(Props[CounterActor], "counterActor")
  import CounterActor._
  (1 to 10).foreach(_ => counterActor ! Increment)
  (1 to 8).foreach(_ => counterActor ! Decrement)
  counterActor ! PrintCounter

  /**
   * Exercise 2
   */
  //Domain of BankAccount
  object BankAccount{
    case class Deposit(amount : Int)
    case class Withdraw(amount : Int)
    case object Statement
    case class TransactionFailure(errorMessage: String)
    case class TransactionSuccess(message : String)

    def props(id: String, amount : Int) = Props(new BankAccount(id, amount))
  }
  import BankAccount._
  class BankAccount(id : String, private var amount : Int) extends Actor {
    val name = s"[Bank Account Actor]"

    override def receive = {
      case Withdraw(money) => {
        if(money < 0) {
          sender ! TransactionFailure(s"Invalid amount $money to withdraw from acccount $id")
        }
        if (money > amount) {
          sender ! TransactionFailure(s"Account $id has not enough balance to withdraw $money from")
        } else {
          amount -= money
          sender ! TransactionSuccess(s"$money is withdrawn from your bank account $id")
        }
      }
      case Deposit(money) => {
        if(money < 0) {
          sender ! TransactionFailure(s"Invalid amount $money to withdraw from acccount $id")
        } else {
          amount += money
          sender ! TransactionSuccess(s"$money is deposited to your bank account $id")
        }
      }
      case Statement => println(s"Your  $id account balance is $amount")
    }
  }

  object AccountHolder {
    def props(name: String, ref: ActorRef) = Props(new AccountHolder(name, ref))
  }
  class AccountHolder(personName : String, ref: ActorRef) extends Actor {
    val name = s"[Account Holder $personName]"

    override def receive = {
      case Withdraw(money) => ref ! Withdraw(money)
      case Deposit(money) => ref ! Deposit(money)
      case Statement => ref ! Statement
      case TransactionFailure(errorMessage) => println(errorMessage)
      case TransactionSuccess(message) => println(message)
    }
  }

  val accountOne = actorSystem.actorOf(BankAccount.props("SBI001",10), "bankAccountOne")
  val accountTwo = actorSystem.actorOf(BankAccount.props("HDFC311",5), "bankAccountTwo")

  val imran = actorSystem.actorOf(AccountHolder.props("Imran", accountOne), "accountHolderOne")
  val sahana = actorSystem.actorOf(AccountHolder.props("Sahana", accountTwo), "accountHolderTwo")

  (1 to 5).foreach{i => if(i % 2 == 0) {
    imran ! Deposit(50)
  } else {
    sahana ! Deposit(100)
  }
  }
  imran ! Statement
  sahana ! Statement
  (1 to 5).foreach {i => if(i % 2 == 0){
    imran ! Withdraw(80)
  } else {
    sahana ! Withdraw(110)
  }
  }
  imran ! Statement
  sahana ! Statement
}
