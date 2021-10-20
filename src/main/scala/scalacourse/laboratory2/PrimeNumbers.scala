package scalacourse.laboratory2



class PrimeNumbers(val n: Int) extends ValueHolder {

  private final val primeArray: Array[Int] = calculatePrimeNumbers(this.n)

  private final def calculatePrimeNumbers(n: Int): Array[Int] = {
    val eratosthenes = new Array[Boolean](n + 1).map(_ => true)
    var i = 0
    var p = 2
    while (p * p <= n) {
      if (eratosthenes(p)) {
        i = p * p
        while (i <= n) {
          eratosthenes(i) = false
          i += p
        }
      }
      p += 1
    }
    eratosthenes.zipWithIndex.filter(x => x._1 && x._2 > 1).collect(x => x._2)
  }

  /**
   * Method returns m-th prime number in range 2 - this.n
   */
  @throws[IndexOutOfBoundsException]
  def number(m: Int): Int = this.primeArray(m)

  override def valueAt(n: Int): Int = number(n);

  def containsIndex(n: Int): Boolean = n >= 0 && n < this.primeArray.length
}

