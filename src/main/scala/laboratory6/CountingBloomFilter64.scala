package laboratory6

import java.util


class CountingBloomFilter64[T](var filterBuilder: FilterBuilder) extends CountingBloomFilter[T] {

  private val MAX = 9223372036854775807L
  private val counters: Array[Long] = new Array[Long](filterBuilder.getSize)
  protected val config: FilterBuilder = this.filterBuilder.complete()
  protected val counts = new util.BitSet(config.getSize * config.getCountingBits)
  protected var bloom: util.BitSet = new util.BitSet(config.getSize)


  override def getConfig: FilterBuilder = this.config

  override def removeAndEstimateCountRaw(element: Array[Byte]): Long = {
    if( !this.contains(element)) return 0L
    var min = Long.MaxValue;
    for (hash <- this.hash(element)) {
      val count = this.decrement(hash)
      this.bloom.set(hash, count > 0L)
      if (count < min) min = count
    }
    min
  }


  override def addAndEstimateCountRaw(element: Array[Byte]): Long = {
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
      this.counters(index) += 1;
      this.counters(index)
    }

  protected def decrement(index: Int): Long =
    if (this.counters(index) == 0L) 0L
    else {
      this.counters(index) -= 1;
      this.counters(index)
    }

  def contains(element: Array[Byte]): Boolean = {
    val var2 = this.hash(element)
    val var3 = var2.length
    for (var4 <- 0 until var3) {
      val position = var2(var4)
      if (!this.bloom.get(position)) return false
    }
    true
  }
}
