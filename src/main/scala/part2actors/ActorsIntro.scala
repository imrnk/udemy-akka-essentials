package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorsIntro extends App {
  //part1 : actor system
  val actorSystem = ActorSystem("FirstActorSystem")

  //part2 : create actors
  //word count actor

  class WordCountActor extends Actor {
    val name = "[word counter]"
    var totalWords = 0

    def receive : PartialFunction[Any, Unit] = {
      case message: String => {
        println(s"$name Received message: $message")
        totalWords += message.split(" ").length
      }
      case other => println(s"$name Cannot understand the ${other.toString}")
    }
  }

  //part 3: instantiate and actor
  val wordCounter = actorSystem.actorOf(Props[WordCountActor], "wordCounter")
  val anotherWordCounter = actorSystem.actorOf(Props[WordCountActor], "AnotherWordCounter")

  //part 4: communicate with the actor
  wordCounter ! "How many words"
  anotherWordCounter ! "A different message"

  class Person(name : String) extends Actor {
    val actorName = "[person actor]"
    override def receive = {
      case "hi" => println(s"$actorName Hi, my name is $name")
      case _ => println(s"$actorName unknown")
    }
  }

  //Its valid but discouraged to instantiate with 'new' inside Props apply method but not possible outside
  //val personActor = actorSystem.actorOf(Props(new Person("Imran")), "personActor")

  //Encouraged method is to create an companion object for the actor that requires an argument
  //to be passed
  object Person{
    def props(name : String) = Props(new Person("name"))
    def apply(name : String) = props(name)
  }
  val personActor = actorSystem.actorOf(Person("Imran"), "personActor")

  personActor ! "hi"
}
