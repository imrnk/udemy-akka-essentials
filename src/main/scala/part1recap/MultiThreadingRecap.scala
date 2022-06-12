package part1recap

object MultiThreadingRecap extends App {

  //creating thread in JVM
  val aThread = new Thread(() => println("I am running in Parallel"))
  aThread.start()
  aThread.join()

  val threadHello = new Thread(() => (1 to 1000).foreach(_ => println("Hello")))
  val threadGoodbye = new Thread(() => (1 to 1000).foreach(_ => println("Goodbye")))

  threadHello.start()
  threadGoodbye.start()
}
