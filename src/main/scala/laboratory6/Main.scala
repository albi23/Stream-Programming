package laboratory6


object Main {

  def main(args: Array[String]): Unit = {

        val cbf: CountingBloomFilter64[Int] = bloomTest(List(1, 2, 3, 4, 5, 6).to(LazyList))
      .asInstanceOf[CountingBloomFilter64[Int]]

    println(cbf)
    for (i <- 0 until 20) {
      print(f"Before remove filter contains $i => ${cbf.contains(i)}, ")
      cbf.remove(i)
      println(f"after remove: $i => ${cbf.contains(i)}")
    }

  }

  def bloomTest[T](input: LazyList[T]): CountingBloomFilter[T] = {
    val cbf: CountingBloomFilter[T] = new FilterBuilder(input.size, 0.01)
      .hashFunction(laboratory6.HashMethod.SHA256)
      .buildCountingBloomFilter[T]
    input.foreach(item => cbf.add(item))
    cbf
  }

}
