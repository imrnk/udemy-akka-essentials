package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.collection.Set

object VotingSystemUsingMutableState extends App {
  val system = ActorSystem("VotingSystemExercise")

  case class Vote(candidate: String)
  case object VoteStatusRequest
  case class VoteStatusResponse(candidate: Option[String])
  case object VotingResult

  class Citizen extends Actor {
    var selectedCandidate: Option[String] = None

    override def receive = {
      case Vote(candidate) => selectedCandidate = Some(candidate)
      case VoteStatusRequest => VoteStatusResponse(selectedCandidate)
    }
  }

  case class AggregateVotes(citizens: Set[ActorRef])

  class VoteAggregator extends Actor {
    override def receive = {
      case AggregateVotes(citizens) => {
        citizens foreach(_ ! VoteStatusRequest)

        context.become(countVotes(Map[String, Int](), citizens))
      }

      def countVotes(poll: Map[String, Int], citizens: Set[ActorRef]) : Receive = {
        case VoteStatusResponse(votedCandidatesOpt) => votedCandidatesOpt match {
        case None => VoteStatusRequest
        case Some(selectedCandidate) =>
          val updatedCount = poll.getOrElse(selectedCandidate, 0) + 1
          val updatedPoll = poll + (selectedCandidate -> updatedCount)
           context.become(countVotes(updatedPoll, citizens - sender))
        }
        case VotingResult =>
          if(citizens.nonEmpty) {
            self forward VotingResult
          } else {
            println(poll)
          }
      }
    }
  }

  val alice = system.actorOf(Props[Citizen], "Alice")
  val bob = system.actorOf(Props[Citizen], "Bob")
  val charlie = system.actorOf(Props[Citizen], "Charlie")
  val daniel = system.actorOf(Props[Citizen], "Daniel")

  alice ! Vote("Martin")
  bob ! Vote("Jonas")
  charlie ! Vote("Roland")
  daniel ! Vote("Roland")

  val voteAggregator = system.actorOf(Props[VoteAggregator], "voteAggregator")

  voteAggregator ! AggregateVotes(Set(alice, bob, charlie, daniel))
  voteAggregator ! VotingResult

}
