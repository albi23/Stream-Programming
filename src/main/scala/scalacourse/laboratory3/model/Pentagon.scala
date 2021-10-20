package scalacourse.laboratory3.model

class Pentagon(side: Double) extends Figure{
  override def perimeter(): Double = 1.25 * side * side * (1.0 / Math.tan(36))
  override def fieldArea(): Double = 5 * side
  override def name(): String = "Pentagon"

}