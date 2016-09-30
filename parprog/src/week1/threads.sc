import week1.{Account, HelloThread}

val tArr = Array.fill(10) {
  new HelloThread
}

tArr.foreach { t => t.start() }
tArr.foreach { t => t.join() }

def startThread(a: Account, b: Account, n: Int) = {
  val t = new Thread {
    override def run: Unit = {
      for (i <- 0 until n) {
        a.transfer(b, 1)
      }
    }
  }
  t.start()
  t
}

val a1 = new Account(50000)
val a2 = new Account(70000)

val t = startThread(a1, a2, 150000)
val s = startThread(a2, a1, 150000)

t.join()
s.join()

