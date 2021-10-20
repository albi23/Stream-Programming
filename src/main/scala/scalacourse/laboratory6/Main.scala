package scalacourse.laboratory6


object Main {

  def main(args: Array[String]): Unit = {

    val testList = List(1, 2, 3, 4, 5, 6).to(LazyList)
    val cbf: CountingBloomFilter64[Int] = bloomTest(testList)

    println(cbf)

    for (i <- 0 until 10) {
      print(f"Before remove filter contains $i => ${cbf.contains(i)}, ")
      cbf.remove(i)
      println(f"after remove: $i => ${cbf.contains(i)}")
    }

  }

  def bloomTest[T](input: LazyList[T]): CountingBloomFilter64[T] = {

    val countingBloomFilter = new CountingBloomFilter64[T](input.size, 0.01, "SHA-256")
    input.foreach(item => countingBloomFilter.add(item))
    countingBloomFilter
  }

}
