package jp.t2v.util.identify

class IdentifierGenerator private (elements: IdentifierElement[_]*) {

  def fold(t: Seq[_]): Long = t.foldLeft(17L) { (a, b) => a * 2909 + hash(b) }
  
  def apply(): String = fold(elements.map(_())).toHexString

  type ToLong = {def toLong: Long}
  
  def hash(value: Any): Long = value match {
    case null => 0
    case l: Long => l
    case a: Array[_] => fold(a)
    case t: ToLong => t.toLong
    case n => n.hashCode
  }
  
}

object IdentifierGenerator {

  def createInstance(elements: IdentifierElement[_]*) = new IdentifierGenerator(elements: _*)
  
  @volatile
  lazy val standard = createInstance(RandomInt, LocalIp, ThreadId, MilliTime, NanoTime)
  
  def apply(): String = standard()
  
}