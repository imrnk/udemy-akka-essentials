package part2actors

import akka.actor.{Actor, ActorRef, ActorSelection, ActorSystem, Props}
import part2actors.ChildActors.CreditCard.{AttachToAccount, CheckStatus}

object ChildActors extends App {

  object Parent{
    case class CreateChild(name: String)
    case class TellChild(message: String)
  }

  class Parent extends Actor {
    import Parent._

    override def receive = {
      case CreateChild(name) =>
        println(s"${self.path} creating child")
        val childRef = context.actorOf(Props[Child], name)
        context.become(withChild(childRef))
    }

    def withChild(childRef: ActorRef) : Receive = {
      case TellChild(content) => childRef ! content
    }
  }

  class Child extends Actor {

    override def receive = {
      case message => println(s"${self.path} I have got: $message")
    }
  }
  import Parent._
  val system = ActorSystem("ParentChildDemo")
  val parent = system.actorOf(Props[Parent], "parent")
  parent ! CreateChild("child")

  parent ! TellChild("Hello Kid!")

  /*
    actor hierarchy

    Guardian actors:
     - /system - system guardian
     - /user - user level guardian
     - / the root guardian
   */

  /**
   * Actor selection
   */
  val childSelection: ActorSelection = system.actorSelection("/user/parent/child")
  childSelection ! "I found you"

  /**
   * Danger :
   * NEVER PASS MUTABLE ACTOR STATE, OR THE `THIS` REFERENCE, TO CHILD ACTORS
   *
   */

  object NaiveBankAccount {
    case class Deposit(amount :Int)
    case class Withdraw(amount : Int)
    case object InitializeAccount
  }

  class  NaiveBankAccount extends Actor {
    import NaiveBankAccount._
    import CreditCard._
    var amount  = 0
    override def receive = {
      case InitializeAccount =>
        val creditCardRef = context.actorOf(Props[CreditCard], "creditcard")
        creditCardRef ! AttachToAccount(this) //!!
      case Deposit(fund) => deposit(fund)
      case Withdraw(fund) => withdraw(fund)
    }

    def deposit(fund: Int) = {
      println(s"${self.path} Depositing $fund on top of $amount")
      amount += fund
    }
    def withdraw(fund: Int) = {
      println(s"${self.path} Depositing $fund on top of $amount")
      amount -= fund
    }
  }

  object CreditCard{
    case class AttachToAccount(bankAccount : NaiveBankAccount) // !!
    case object CheckStatus
  }

  class CreditCard extends Actor {
    import CreditCard._
    override def receive = {
      case AttachToAccount(account) => context.become(attachedToAccount(account))
    }

    def attachedToAccount(account: ChildActors.NaiveBankAccount) : Receive = {
      case CheckStatus => println(s"${self.path} Your message has processed")
        account.withdraw(1)
    }
  }
  import NaiveBankAccount._
  val bankAccountRef = system.actorOf(Props[NaiveBankAccount], "bankAccount")
  bankAccountRef ! InitializeAccount
  bankAccountRef ! Deposit(100)


  Thread.sleep(500)
  val ccSelection = system.actorSelection("/user/bankAccount/creditcard")
  ccSelection ! CheckStatus
}
