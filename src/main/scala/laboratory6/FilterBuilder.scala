package laboratory6

import laboratory6.HashMethod.HashFunction

class FilterBuilder(var expectedElements: Int,
                    var falsePositiveProbability: Double) {

  private var done: Boolean = false
  private var size: Int = -1
  private var hashes: Int = -1
  private var hashMethod: HashMethod.HashMethodImpl = HashMethod.RNG
  private var hashFunction: HashFunction = null
  private val countingBits: Int = 64


  def optimalM(n: Long, p: Double): Int = Math.ceil(-1.0D * n.toDouble * Math.log(p) / Math.pow(Math.log(2.0D), 2.0D)).toInt

  def optimalK(n: Long, m: Long): Int = Math.ceil(Math.log(2.0D) * m.toDouble / n.toDouble).toInt

  def expectedElements(expectedElements: Int): FilterBuilder = {
    this.expectedElements = expectedElements
    this
  }

  def falsePositiveProbability(falsePositiveProbability: Double): FilterBuilder = {
    this.falsePositiveProbability = falsePositiveProbability
    this
  }

  def hashFunction(hashMethod: HashMethod.HashMethodImpl): FilterBuilder = {
    this.hashMethod = hashMethod
    this.hashFunction = hashMethod.getHashFunction
    this
  }

  def complete(): FilterBuilder = {
    if (this.done) this
    else {
      if (this.size == -1 && this.expectedElements != null && this.falsePositiveProbability != null)
        this.size = optimalM(this.expectedElements.asInstanceOf[Long], this.falsePositiveProbability)

      if (this.hashes == -1 && this.expectedElements != null && this.size != -1)
        this.hashes = optimalK(this.expectedElements.asInstanceOf[Long], this.size.asInstanceOf[Long])

      if (this.hashes == -1 && this.size == -1) {
        throw new IllegalStateException(f"You must set hashes and size")
      }
      this.done = true
      this
    }
  }

  def buildCountingBloomFilter[T]: CountingBloomFilter[T] = {
    this.complete()
    new CountingBloomFilter64[T](this).asInstanceOf[CountingBloomFilter[T]]
  }

  def getCountingBits: Int = {
    this.countingBits
  }

  def getSize: Int = {
    this.size
  }

  def getHashFunction: HashFunction ={
    this.hashFunction
  }

  def getHashes: Int ={
    this.hashes
  }
}
