package laboratory6

import java.security.{MessageDigest, NoSuchAlgorithmException}
import java.util
import java.util.Random

object HashProvider {

  def hashRNG(value: Array[Byte], m: Int, k: Int): Array[Int] = {
    val positions = new Array[Int](k)
    val r = new Random(hashBytes(value).toLong)
    for (i <- 0 until k) {
      positions(i) = r.nextInt(m)
    }
    positions
  }

  private def hashBytes(a: Array[Byte]) = {
    val FNV_PRIME = 16777619L
    val FNV_OFFSET_BASIS = 2166136261L
    if (a == null) 0
    else {
      var result = FNV_OFFSET_BASIS
      val var7 = a
      val var8 = a.length
      for (var9 <- 0 until var8) {
        val element = var7(var9)
        result = result * FNV_PRIME & -1L
        result ^= element.toLong
      }
      result.toInt
    }
  }

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

}
