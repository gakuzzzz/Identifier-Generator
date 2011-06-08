package jp.t2v.util.identify

import collection.mutable.StringBuilder
import annotation.tailrec

class IdentifierGenerator private (elements: IdentifierElement[_]*) {

  private val table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".toCharArray
  private val mask = 0x3f
  private val bitPerChar = 6

  def apply(): String = fold(elements.map(_()))

  private def fold(t: Seq[_]): String = {
    val encoded = new StringBuilder
    val (rr, ss) = t.foldLeft((0, 0)) { case ((r, s), v) => encode(v, r, s, encoded) }
    if (ss > 0) encoded += table(rr & mask)
    encoded.toString()
  }

  private def encode(value: Any, previous: Int, residualBitSize: Int, encoded: StringBuilder): (Int, Int) = value match {
    case l: Long =>
      val (r, s) = encode(l.toInt, 32, previous, residualBitSize, encoded)
      encode((l >>> 32).toInt, 32, r, s, encoded)
    case i: Int => encode(i, 32, previous, residualBitSize, encoded)
    case a: Array[Byte] => a.foldLeft((previous, residualBitSize)) {
      case ((r, s), v) => { encode(0xff & v, 8, r, s, encoded) }
    }
  }

  private def encode(current: Int, currentBitSize: Int, previous: Int, residualBitSize: Int, encoded: StringBuilder): (Int, Int) = {
      encoded += table(((current << residualBitSize) | previous) & mask)
      val used = bitPerChar - residualBitSize
      encode(current >>> used, currentBitSize - used, encoded)
  }

  @tailrec
  private def encode(bits: Int, size: Int, encoded: StringBuilder): (Int, Int) = {
    if (size < bitPerChar) {
      (bits, size)
    } else {
      encoded += table(bits & mask)
      encode(bits >>> bitPerChar, size - bitPerChar, encoded)
    }
  }

}

object IdentifierGenerator extends (() => String) {

  def createInstance(elements: IdentifierElement[_]*) = new IdentifierGenerator(elements: _*)

  @volatile
  private lazy val standard = createInstance(RandomInt, NanoTime, MilliTime, ThreadId, ProcessId, LocalIp)

  def apply(): String = standard()

  def main(args: Array[String]) {
    println(IdentifierGenerator())
  }

}