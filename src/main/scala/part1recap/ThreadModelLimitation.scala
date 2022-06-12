package part1recap

import scala.concurrent.Future

object ThreadModelLimitation extends App {

  /**
   * #1   OOP encapsulation is only valid in single threaded model
   *
   * involves synchronization
   */
  class BankAccount(private var amount: Int) {
    override def toString = s"$amount"

    def getAmount = this.amount

    def withdraw(money: Int) = this.amount -= money

    def deposit(money: Int) = this.amount += money
  }

  val account = new BankAccount(2000)
/*

  for (_ <- 1 to 1000) {
    new Thread(() => account.withdraw(1)).start()
  }

  for (_ <- 1 to 1000) {
    new Thread(() => account.deposit(1)).start()
  }
*/

 // println(account.getAmount)

  /**
   * DR #2: Delegating something to a thread is difficult
   */

  //you have a running thread and you want to pass a Runnable to that thread
  /*var task: Runnable = null

  val runningThread: Thread = new Thread {
    () => while (true) {
      while (task == null) {
        runningThread.synchronized {
          println("[background] waiting for a task...")
          runningThread.wait()
        }
      }
      task.synchronized{
        println("[background] I have a task...")
        task.run()
        task = null
      }
    }
  }

  def delegateToBackgroundThread(r: Runnable) = {
    if(task == null) task = r

    runningThread.synchronized{
      runningThread.notify()
    }
  }

  runningThread.start()
  Thread.sleep(1000)
  delegateToBackgroundThread(() => println(42))
  Thread.sleep(1000)
  delegateToBackgroundThread(() => println("This should run in the background"))
*/
  /**
   * DR 3: tracking and dealing with errors in multithreaded env is extremely difficult
   */
  // 1M numbers in between 10 threads
  import scala.concurrent.ExecutionContext.Implicits.global

  val futures = (1 to 10).map {
    i => (100000 * i) until (100000 * (i + 1)) //0 - 99999, 100000 - 199999
  }.map{range => Future{
    if(range.contains(456324)) throw new RuntimeException("invalid number")
    range.sum
  }}

  val sumFutures = Future.reduceLeft(futures)(_ + _) //Future of sum of all numbers
  sumFutures.onComplete(println)
}
