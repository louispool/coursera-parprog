package week1

class HelloThread extends Thread {

  override def run(): Unit = {
    import HelloThread._
    println("Hello World!" + getUniqueId)
  }
}

object HelloThread {

  //Synchronized access to shared resource
  private var uidCount = 0L
  private val monitor  = new AnyRef {}

  def getUniqueId = monitor.synchronized {
    uidCount = uidCount + 1
    uidCount
  }

}


