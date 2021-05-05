package laboratory6

import java.nio.charset.Charset

trait BloomFilter[T] {

  val defaultCharset: Charset = Charset.forName("UTF-8")

  def addRaw(element: Array[Byte]): Boolean = this.addAndEstimateCountRaw(element) == 1L

  def add(element: T): Boolean = this.addRaw(this.toBytes(element))

  def toBytes(element: T): Array[Byte] = element.toString.getBytes(defaultCharset)

  def addAndEstimateCountRaw(element: Array[Byte]): Long

  def getConfig: FilterBuilder

  def hash(bytes: Array[Byte]): Array[Int] = this.getConfig.getHashFunction.hash(bytes, this.getConfig.getSize, this.getConfig.getHashes)
}
