package scalacourse.laboratory2

trait ValueHolder {

  def containsIndex(n: Int): Boolean

  @throws[IndexOutOfBoundsException]
  def valueAt(n: Int): Int
}
