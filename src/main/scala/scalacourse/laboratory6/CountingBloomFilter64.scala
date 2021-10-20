package scalacourse.laboratory6

import java.nio.charset.{Charset, StandardCharsets}
import java.security.{MessageDigest, NoSuchAlgorithmException}
import java.util
import java.util.Random


class CountingBloomFilter64[T](
                                private var expectedElements: Int = 1,
                                private var falsePositiveProbability: Double = 0.01D,
                                private val hashMethod: String = "SHA-256"
                              ) {

  private val MAX = 9_223_372_036_854_775_807L
  protected val size: Int = optimalSize(this.expectedElements.asInstanceOf[Long], this.falsePositiveProbability)
  protected val hashes: Int = optimalHashAmount(this.expectedElements.asInstanceOf[Long], this.size.asInstanceOf[Long])

  private val counters: Array[Long] = new Array[Long](this.size)
  protected var bloom: util.BitSet = new util.BitSet(this.size)

  val charset: Charset = Charset.forName(StandardCharsets.UTF_8.name())


  def toBytes(element: T): Array[Byte] = element.toString.getBytes(charset)


  def remove(element: T): Boolean = {
    this.removeAndEstimateCountRaw(this.toBytes(element)) <= 0L
  }

  def removeAndEstimateCountRaw(element: Array[Byte]): Long = {
    if (!this.contains(element)) return 0L
    var min = Long.MaxValue;
    for (hash <- this.hash(element)) {
      val count = this.decrement(hash)
      this.bloom.set(hash, count > 0L)
      if (count < min) min = count
    }
    min
  }

  def add(element: T): Boolean = {
    addAndEstimateCountRaw(this.toBytes(element)) == 1L
  }

  def addAndEstimateCountRaw(element: Array[Byte]): Long = {
    var min = Long.MaxValue
    for (hash <- this.hash(element)) {
      this.bloom.set(hash, true)
      val count = this.increment(hash)
      if (count < min) min = count
    }
    min
  }

  protected def increment(index: Int): Long =
    if (this.counters(index) == MAX) MAX
    else {
      this.counters(index) += 1
      this.counters(index)
    }

  protected def decrement(index: Int): Long =
    if (this.counters(index) == 0L) 0L
    else {
      this.counters(index) -= 1
      this.counters(index)
    }

  def contains(element: T): Boolean = this.contains(this.toBytes(element))


  def contains(element: Array[Byte]): Boolean = {
    val hash = this.hash(element)
    val hashLength = hash.length
    for (i <- 0 until hashLength) {
      val position = hash(i)
      if (!this.bloom.get(position)) return false
    }
    true
  }

  protected def count(index: Int): Long = {
    this.counters(index)
  }

  def hash(bytes: Array[Byte]): Array[Int] = hashCrypt(bytes, this.size, this.hashes, this.hashMethod)


  def hashCrypt(value: Array[Byte], m: Int, k: Int, method: String): Array[Int] = {
    var cryptHash: MessageDigest = null
    try cryptHash = MessageDigest.getInstance(method)
    catch {
      case ex: NoSuchAlgorithmException =>
        throw new RuntimeException(ex)
    }
    val positions: Array[Int] = new Array[Int](k)
    var computedHashes: Int = 0
    new Random(89478583L)
    var digest: Array[Byte] = new Array[Byte](0)
    while (computedHashes < k) {
      cryptHash.update(digest)
      digest = cryptHash.digest(value)
      val hashed: util.BitSet = util.BitSet.valueOf(digest)
      val filterSize: Int = 32 - Integer.numberOfLeadingZeros(m)
      val hashBits: Int = digest.length * 8
      var split: Int = 0
      while (split < hashBits / filterSize && computedHashes < k) {
        val from: Int = split * filterSize
        val to: Int = (split + 1) * filterSize
        val hashSlice: util.BitSet = hashed.get(from, to)
        val longHash: Array[Long] = hashSlice.toLongArray
        val intHash: Int = if (longHash.length > 0) longHash(0).toInt else 0
        if (intHash < m) {
          positions(computedHashes) = intHash
          computedHashes += 1
        }

        split += 1
      }
    }
    positions
  }


  def optimalSize(n: Long, p: Double): Int = Math.ceil(-1.0D * n.toDouble * Math.log(p) / Math.pow(Math.log(2.0D), 2.0D)).toInt

  def optimalHashAmount(n: Long, m: Long): Int = Math.ceil(Math.log(2.0D) * m.toDouble / n.toDouble).toInt

  override def toString = s"CountingBloomFilter64{" +
    s"\ncounters=${counters.mkString("[", ", ", "]")},\n" +
    s"config={size=$size, hashes=$hashes, hashMethod=$hashMethod, countingBits=64, expectedElements=$expectedElements, falsePositiveProbability=$falsePositiveProbability}\n" +
    s"bloom=${bloom.toString})}"
}
