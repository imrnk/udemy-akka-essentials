package part2actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

object IntroAkkaConfig extends App {

  class SimpleLoggingActor extends Actor with ActorLogging{
    override def receive = {
      case message => log.info(message.toString)
    }
  }
  /**
   * 1: Inline configuration
   */
  val configString =
    """
      |akka {
      | loglevel = "ERROR"
      |}
      |""".stripMargin

  val config = ConfigFactory.parseString(configString)
  val system = ActorSystem("ConfigurationDemo", ConfigFactory.load(config))

  val simpleLogging = system.actorOf(Props[SimpleLoggingActor], "simpleLogging")
  simpleLogging ! "A message to remember"

  /**
   * 2 - Default config file
   */
  val defaultFileConfigSystem = ActorSystem("DefaultFileConfigDemo")
  val defaultConfigActor = defaultFileConfigSystem.actorOf(Props[SimpleLoggingActor], "defaultLogging")

  defaultConfigActor ! "Remember Me"

  /**
   * 3 - Separate configuration in the same file
   */
  val specialConfig = ConfigFactory.load().getConfig("myspecialconfig")
  val specialConfigSystem = ActorSystem("SpecialConfigDemo", specialConfig)

  val specialConfigActor = specialConfigSystem.actorOf(Props[SimpleLoggingActor], "specialConfig")

  specialConfigActor ! "Special Actor"

  /**
   * 4 - Separate configuration in another file
   */
  val secretConfig = ConfigFactory.load("secret/secretconfig.conf")

  /**
   * 5 - Different file format
   * JSON
   * PROPERTIES
   */

  val jsonConfig = ConfigFactory.load("json/jsonconfig.json")
  println(s"json config : ${jsonConfig.getString("key")}")
  println(s"json config : ${jsonConfig.getString("akka.loglevel")}")


}
