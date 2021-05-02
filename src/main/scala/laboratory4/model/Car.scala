package laboratory4.model

class Car(val mark: String) {

  override def equals(other: Any): Boolean =
    other.isInstanceOf[Car] && this.mark == other.asInstanceOf[Car].mark

  override def hashCode(): Int = {
    Seq(mark).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = f"Car{mark=$mark}"
}
