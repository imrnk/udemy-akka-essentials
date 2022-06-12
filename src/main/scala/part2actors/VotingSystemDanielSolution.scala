package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem}

object VotingSystemDanielSolution extends App {

  val system = ActorSystem("VotingSystemExercise")

  case class Vote(candidate: String)
  case object VoteStatusRequest
  case class VoteStatusResponse(candidate: Option[String])
  case object VotingResult

  class Citizen extends Actor {

    override def receive = ???

  }

  case class AggregateVotes(citizens: Set[ActorRef])

  class VoteAggregator extends Actor {
    override def receive = ???

  }
}
