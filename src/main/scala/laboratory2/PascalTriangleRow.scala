package laboratory2

import scala.collection.mutable.ArrayBuffer

class PascalTriangleRow(val n: Int) extends ValueHolder {

  private final val pascalArr: ArrayBuffer[Int] = calculate(this.n)

  /**
   * Method construct n-th row of pascal triangle
   */
  private final def calculate(rowIndex: Int): ArrayBuffer[Int] = {
    val currRow = new ArrayBuffer[Int]()
    currRow += 1
    if (rowIndex == 0) return currRow
    val prev = calculate(rowIndex - 1)
    for (i <- 1 until prev.size) {
      val curr = prev(i - 1) + prev(i)
      currRow += curr
    }
    currRow += 1
    currRow
  }


  /**
   * Method returns m-th number of n-th line in pascal triangle
   */
  @throws[IndexOutOfBoundsException]
  def factor(m: Int): Int = this.pascalArr(m)

  override def valueAt(n: Int): Int = factor(n);

  def containsIndex(n: Int): Boolean = n >= 0 && n < this.pascalArr.length
}
