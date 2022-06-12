package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChildActorsExercises extends App{

  //distributed word counting

  object WordCounterMaster {
    case class Initialize(nChildren: Int)
    case class Text(str: String)
    case class WordCountTask(workerRef: ActorRef, text : String)
    case class WordCountReply(workerRef: ActorRef, count: Int)
  }

  class WordCounterMaster extends Actor {
    import WordCounterMaster._

    var runningWorker : Seq[ActorRef] = Seq()
    var freeWorker : Seq[ActorRef] = Seq()

    override def receive = {
      case Initialize(nChild) =>
        freeWorker = (1 to nChild).map(n => context.actorOf(Props[WordCounterWorker], s"wcw$n"))
        println(s"${freeWorker.length} workers initialized")

      case Text(str) => {
          if(freeWorker.nonEmpty) {
            val firstFree = freeWorker.take(1)
            println(s"firstFree $firstFree")
            runningWorker = runningWorker ++ firstFree
            freeWorker = freeWorker.filterNot(_ == firstFree.head)
            println(s"freeWorker now ${freeWorker.map(_.path)}")
            println(s"runningWorker now ${runningWorker.map(_.path)}")
            firstFree.head ! WordCountTask(firstFree.head, str)
          } else
            self forward  Text(str) //infinite loop in the hope that sometime in future one of the worker will be free
      }
      case WordCountTask(workerRef, text) => {
        println(s"${workerRef.path} taking up the task of counting words in '${text.substring(0,10)}...'")
        workerRef ! text
      }
      case WordCountReply(workerRef, count) => {
        runningWorker = runningWorker.filterNot(_ == workerRef)
        println(s"runningWorker updated ${runningWorker.map(_.path)}")
        freeWorker = freeWorker ++ Seq(workerRef)
        println(s"freeWorker updated ${freeWorker.map(_.path)}")
        println(s"${workerRef.path} Counted the words: $count")
      }
    }
  }

  class WordCounterWorker extends Actor {
    import WordCounterMaster._
    override def receive = {
      case WordCountTask(ref, text) => {
        println(s"${self.path} is tasked with '${text.substring(0,10)}...'")
        println(s"Received Worker ref: ${ref.path}")

        sender() ! WordCountReply(ref, text.split(" ").length)
      }
    }
  }

  import WordCounterMaster._

  val system = ActorSystem("WordCountMasterWorkersExercise")
  val master = system.actorOf(Props[WordCounterMaster], "master")

  master ! Initialize(3)
  master ! Text("The Teams page contains a listing of the various Community Teams, their responsibilities, links to their Wiki Home Pages and leaders, communication tools, and a quick reference to let you know whether and when they hold meetings.Most Teams Wiki Home Pages provide information about who they are, what they do, when their meetings are, and how to contact them. Using these pages, teammates are able to communicate and coordinate projects. LoCoTeams For participating on the Country area team contributing to a Local Development of Localization and Internationalization and promoting use of Ubuntu.Governance and Membership Like most communities, we have our rules and governing body.\n\nAnyone can join and participate in most, if not all, of our Teams and Projects. But if you want an @ubuntu.com e-mail address, it has to be earned. Find out how in our Membership section.")
  master ! Text("Ratnagiri is a major Railhead on Konkan Railway route. Ratnagiri is also one of the two divisions of Konkan Railway Corporation. The city is well connected to Mumbai, New Delhi, Amritsar, Chandigarh, Dehradun, Jaipur, Jodhpur, Bikaner, Ahmedabad, Vadodara, Surat, Bhuj, Indore, Jabalpur, Patna, Nagpur, Pune, Margao, Mangalore, Kochi, Thiruvananthapuram, Kanyakumari, Coimbatore and other major towns of the country. Every train passing through the city halts here. Connectivity to western Maharashtra is proposed through Vaibhavwadi Road – Kolhapur route.")
  master ! Text("long-lived, and very erratic tropical cyclone, Leslie developed from an extratropical cyclone that was situated over the northern Atlantic on 22 September. It became a Category 1 hurricane early on 3 October before falling to tropical storm intensity late on 4 October. After re-intensifying, Leslie reached hurricane status for the second time on 10 October, reaching peak intensity two days later and passing between the Azores and Madeira")
  master ! Text("was a British photographer who is considered to be one of the most important portraitists of the 19th century. She is known for her soft-focus close-ups of famous Victorian men and for illustrative images depicting characters from mythology, Christianity and literature. Cameron also produced sensitive portraits of women and children. After showing a keen interest in photography for many years, she took up the practice at the relatively late age of 48, when her daughter gave her a camera as a present. She quickly produced a large body of work capturing the genius, beauty, and innocence of the men, women, and children who visited her studio, and created unique allegorical images inspired by tableaux vivants, theatre, 15th-century Italian painters, and the work of her creative contemporaries")
  master ! Text("Afonso died from epilepsy at the age of two, devastating the emperor. The following year, Pedro and Teresa Cristina had another son, Pedro Afonso, but he too died in infancy. After the loss of his second son, doubts grew in Pedro II's mind that the imperial system could be viable. He still had an heir in his daughter Isabel, but he was unconvinced that a female would prove to be a suitable successor. He showed less concern about the effects his policies had on the monarchy, provided his daughter Isabel with no training for her role as potential empress, and failed to cultivate her acceptance within the country's political class. Pedro II's lack of interest in protecting the imperial system ultimately led to its downfall.")

  /*
   create wordcountermaster
   send initialize(10) to wordcountermaster
   send "akka is awesome" to wordcountermaster
   wcm will send a WordCountTask("...") to one of its worker
    child reply with a WordCountReply(3) to the master
    master will reply 3  to the sender

    requester -> wcm -> wcw -> wcm -> requester

    //round robin logic
   */

}
