package week1

import HelloThread._

class Account(private var amount: Int = 0) {

  val uid = getUniqueId

  private def lockAndtransfer(target: Account, n: Int) = {
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }
  }

  def transfer(target: Account, n: Int) = {
    //Resolve Thread Deadlock via ordering
    if (this.uid < target.uid) this.lockAndtransfer(target, n)
    else target.lockAndtransfer(this, -n)
  }
}
