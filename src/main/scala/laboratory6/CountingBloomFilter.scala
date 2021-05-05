package laboratory6

trait CountingBloomFilter[T] extends BloomFilter[T] {

  def removeRaw(element: Array[Byte]): Boolean = this.removeAndEstimateCountRaw(element) <= 0L

  def remove(element: T): Boolean = this.removeRaw(this.toBytes(element))

  def removeAndEstimateCount(element: T): Long = this.removeAndEstimateCountRaw(this.toBytes(element))

  def removeAndEstimateCountRaw(element: Array[Byte]): Long
}
