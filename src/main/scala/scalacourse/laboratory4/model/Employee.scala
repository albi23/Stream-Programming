package scalacourse.laboratory4.model

class Employee(private val salary: Double) {
  private final val hash: Int =  Seq(salary).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def equals(other: Any): Boolean =
    other.isInstanceOf[Employee] && this.salary == other.asInstanceOf[Employee].salary

  override def hashCode(): Int = hash

  override def toString: String = f"Employee{salary=$salary}"
}
