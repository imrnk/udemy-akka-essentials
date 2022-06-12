package part2actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.event.Logging

object ActorLoggingDemo extends App{

  class SimpleActorWithExplicitLogger extends Actor {
    val logger = Logging(context.system, this)

    /*
     1 - DEBUG
     2 - INFO
     3 - WARN
     4 - ERROR
     */
    override def receive = {
      case message => logger.info(message.toString)
    }
  }

  class SimpleActorWithActorLog extends Actor with ActorLogging {
    override def receive = {
      case(a, b) => log.info("Two things: {} and {}", a , b)
      case message => log.info(message.toString)
    }
  }

  val system = ActorSystem("ExplicitLogDemo")
  val explicitLogger = system.actorOf(Props[SimpleActorWithExplicitLogger], "explicitLogger")
  explicitLogger ! "Logging a message"

  val simpleLogger = system.actorOf(Props[SimpleActorWithActorLog], "simpleLogger")

  simpleLogger ! "Logging another message"

  simpleLogger ! ("This", "That")
}
