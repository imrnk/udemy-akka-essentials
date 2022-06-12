package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ChildActorsExercises.WordCounterMaster

object ChildActorsExercisesDaniel extends  App{

  //distributed word counting

  object WordCounterMaster {
    case class Initialize(nChildren: Int)
    case class Text(str: String)
    case class WordCountTask(id: Int, text : String)
    case class WordCountReply(id: Int, count: Int)
  }

  class WordCounterMaster extends Actor {

    import WordCounterMaster._

    override def receive = {
      case Initialize(nChildren) =>
        println(s"[master] Initializing...")
        val childrenRefs = for (i <- 1 to nChildren) yield context.actorOf(Props[WordCounterWorker], s"wcw$i")
        context.become(withChildren(childrenRefs, 0, 0, Map()))
    }

    def withChildren(children: Seq[ActorRef], currenChildIndex : Int, currentTaskId: Int, requestMap: Map[Int, ActorRef]) : Receive = {
      case text : String =>
        println(s"[master] I have received $text - I will send it to child $currenChildIndex")
        val originalSender = sender()
        val task = WordCountTask(currentTaskId, text)
        val childRef = children(currenChildIndex)
        childRef ! task

        val nextChildRefIndex = (currenChildIndex + 1) % children.length
        val newTaskId = currentTaskId + 1
        val newRequestMap = requestMap + (currentTaskId -> originalSender)
        context.become(withChildren(children, nextChildRefIndex, newTaskId, newRequestMap))

      case WordCountReply(id, count) =>
        println(s"[master] I have received a reply for task $id with count $count")
        val originalSender = requestMap(id)
        originalSender ! count
        context.become(withChildren(children, currenChildIndex, currentTaskId, requestMap - id))
    }
  }

  class WordCounterWorker extends Actor {

    import WordCounterMaster._

    override def receive = {
      case WordCountTask(id, text) =>
        println(s"[worker] ${self.path} I have received task $id with $text")
        sender ! WordCountReply(id, text.split(" ").length)
    }
  }

  class TestActor extends Actor {
    import WordCounterMaster._
    override def receive = {
      case "go" =>
        val master = context.actorOf(Props[WordCounterMaster], "master")
        master ! Initialize(3)
        val texts = Seq("I love akka", "Scala is super dope", "yes","was a British photographer who is considered to be one of the most important portraitists of the 19th century. She is known for her soft-focus close-ups of famous Victorian men and for illustrative images depicting characters from mythology, Christianity and literature. Cameron also produced sensitive portraits of women and children. After showing a keen interest in photography for many years, she took up the practice at the relatively late age of 48, when her daughter gave her a camera as a present. She quickly produced a large body of work capturing the genius, beauty, and innocence of the men, women, and children who visited her studio, and created unique allegorical images inspired by tableaux vivants, theatre, 15th-century Italian painters, and the work of her creative contemporaries" ,"me too", "The Teams page contains a listing of the various Community Teams, their responsibilities, links to their Wiki Home Pages and leaders, communication tools, and a quick reference to let you know whether and when they hold meetings.Most Teams Wiki Home Pages provide information about who they are, what they do, when their meetings are, and how to contact them. Using these pages, teammates are able to communicate and coordinate projects. LoCoTeams For participating on the Country area team contributing to a Local Development of Localization and Internationalization and promoting use of Ubuntu.Governance and Membership Like most communities, we have our rules and governing body.\n\nAnyone can join and participate in most, if not all, of our Teams and Projects. But if you want an @ubuntu.com e-mail address, it has to be earned. Find out how in our Membership section.")
        texts foreach (text => master ! text)
      case count : Int =>
        println(s"[test actor] I received reply : $count")

    }
  }

  val system = ActorSystem("RoundRobinWordCount")
  val testActor = system.actorOf(Props[TestActor], "testActor")
  testActor ! "go"

}
