package laboratory5

import laboratory4.model.Employee

import scala.collection.mutable

object SpaceSavingAlgorithm {


  def main(args: Array[String]): Unit = {
    val input = List(1, 1, 2, 1, 2, 3, 4, 2, 1, 2, 1, 2).map(x => new Employee(x))
    println(MirsaGriesAlgorithm1(input.to(LazyList), 3))

    val input2 = List(1, 2, 4, 3, 4, 3, 4, 5, 4, 6, 7, 3, 3, 6, 1, 1, 3, 2, 4, 7).map(x => new Employee(x))
    println(MirsaGriesAlgorithm2(input2.to(LazyList), 0.2))
  }

  /**
   * Space saving version
   */
  private def MirsaGriesAlgorithm1(input: LazyList[Any], k: Int): mutable.Map[Any, Int] = {
    if (k <= 0) throw new IllegalArgumentException("Param k should be greater than 0")
    val histogram: mutable.Map[Any, Int] = mutable.Map[Any, Int]()
    input.foreach(param => {
      if (histogram.contains(param)) {
        histogram(param) += 1
      } else if (histogram.size < k) {
        histogram.put(param, 1)
      } else {
        var min: (Any, Int) = ("", Int.MaxValue)
        for ((k, v) <- histogram) {
          if (v < min._2) {
            min = (k, v)
          }
        }
        histogram += param -> (histogram.remove(min._1).get + 1)
      }
    })
    histogram
  }

  /**
   * Lossy counting algorithm
   * @param input - data input stream
   * @param e     - epsilon
   * @return      - map
   */
  private def MirsaGriesAlgorithm2(input: LazyList[Any], e: Double): mutable.Map[Any, (Int, Int)] = {
    if (e <= 0 || e > 1) throw new IllegalArgumentException("Param e should be between 0 and 1")
    val bucketSize: Int = (1 / e).toInt
    var currBucket = 1
    var n = 0
    val statistics: mutable.Map[Any, (Int, Int)] = mutable.Map[Any, (Int, Int)]()
    input.foreach(x => {
      n += 1
      if (statistics.contains(x)) {
        statistics.update(x, (statistics(x)._1 + 1, statistics(x)._2))
      } else {
        statistics.put(x, (1, currBucket - 1))
      }
      if (n % bucketSize == 0) {
        statistics.keys.foreach(k => {
          val tuple = statistics(k)
          if (tuple._1 + tuple._2 <= currBucket) {
            statistics.remove(k)
          }
        })
        currBucket += 1
      }
    })
    statistics
  }


}
