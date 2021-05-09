package laboratory4

import laboratory4.model.{Book, Car, Employee}

import scala.collection.mutable


object MirsaGriesMain {

  def main(args: Array[String]): Unit = {

    val input = List(
      new Car("mazda"), new Car("mazda"), new Car("mazda"), new Car("mazda"), new Car("mazda"),
      new Book("book"), new Book("book"), new Book("book"), new Book("book"), new Book("book"),
      new Employee(1), new Employee(1), new Employee(1), new Employee(1), new Employee(1),
    )
    println(MirsaGriesAlgorithm(input.to(LazyList), 5))
  }

  private def MirsaGriesAlgorithm[T](input: LazyList[T], k: Int): mutable.Map[T, Int] = {
    if (k <= 0) throw new IllegalArgumentException("Param k should be greater than 0")
    val histogram: mutable.Map[T, Int] = mutable.Map[T, Int]()

    input.foreach(param => {
      if (histogram.contains(param)) {
        histogram.update(param, histogram(param) + 1)
      } else if (histogram.size < (k - 1)) {
        histogram.put(param, 1)
      } else {
        histogram.keys.foreach(k => if (histogram(k) != 1) histogram(k) -= 1 else histogram.remove(k))
      }
    })
    histogram
  }
}
