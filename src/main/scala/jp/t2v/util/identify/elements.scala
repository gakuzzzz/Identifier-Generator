package jp.t2v.util.identify

trait IdentifierElement[A] {
  def apply(): A
}

object ThreadId extends IdentifierElement[Long] {
  def apply = Thread.currentThread.getId
}

object MilliTime extends IdentifierElement[Long] {
  def apply = System.currentTimeMillis
}

object NanoTime extends IdentifierElement[Long] {
  def apply = System.nanoTime
}

object RandomInt extends IdentifierElement[Long] {
  import util.Random

  val random = new ThreadLocal[Random] {
    override def initialValue = new Random
  }
  
  def apply = random.get.nextLong
 
}

object LocalIp extends IdentifierElement[Array[Byte]] {
  import java.net.InetAddress._

  @volatile
  lazy val apply = getLocalHost.getAddress

}