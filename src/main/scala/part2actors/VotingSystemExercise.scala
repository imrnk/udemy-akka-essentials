package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object VotingSystemExercise extends App {

  /**
   * Exercise 2: A simplified voting system
   *
   *
   */

  val system = ActorSystem("VotingSystemExercise")

  case class Vote(candidate: String)
  case object VoteStatusRequest
  case class VoteStatusResponse(candidate: Option[String])
  case object VotingResult

  class Citizen extends Actor {

    //When Citizen will be invoked from outside the selectedCandidate will be None
    override def receive = receiveVotingMessageFromAggregator(None)

    def receiveVotingMessageFromAggregator(selectedCandidateOpt: Option[String]) : Receive = {
      case Vote(candidate) if selectedCandidateOpt.isEmpty => context.become(receiveVotingMessageFromAggregator(Some(candidate)))
      case VoteStatusRequest => sender ! VoteStatusResponse(selectedCandidateOpt)
    }
  }

  case class AggregateVotes(citizens: Set[ActorRef])

  class VoteAggregator extends Actor {
    override def receive = {
      case AggregateVotes(refs) => {
        //Request each citizens to send individual Vote status
        refs foreach(_ ! VoteStatusRequest)

        //Then switch context to start count votes by passing an empty map and the Citizen's refs
        context.become(countVote(Map[String, Int](), refs))
      }

      def countVote(poll: Map[String, Int], refs: Set[ActorRef]) : Receive = {
        //If the citizen has not voted yet keep sending request for vote status
        case VoteStatusResponse(None) => sender ! VoteStatusRequest
        case VoteStatusResponse(Some(selectedCandidate)) => {
          //Increment the count of votes for selectedCandidate and update the Map
          val voteCountForThisSelectedCandidate = poll.getOrElse(selectedCandidate, 0) + 1
          val updatedPoll = poll + (selectedCandidate -> voteCountForThisSelectedCandidate)
          //call the countVote again with updated Map with the Citizen removed from the Set
          context.become(countVote(updatedPoll, refs - sender))
        }
        case VotingResult => {
          //self loop until all the citizen voted
          if(refs.nonEmpty) self forward VotingResult
          else
            println(s"$self Poll Result : $poll")
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
  /**
   * Print the status of votes
   * Result: Map(Martin -> 1, Jonas -> 1, Roland -> 2)
   */

}
