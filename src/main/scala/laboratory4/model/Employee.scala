package laboratory4.model

class Employee(val salary: Double) {
  override def equals(other: Any): Boolean =
    other.isInstanceOf[Employee] && this.salary == other.asInstanceOf[Employee].salary

  override def hashCode(): Int = {
    Seq(salary).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = f"Employee{salary=$salary}"
}
