package scalacourse.laboratory3.model

class Hexagon(side: Double) extends Figure{
  override def perimeter(): Double = 1.5 * side * side * Math.sqrt(3)
  override def fieldArea(): Double = 6 * side
  override def name(): String = "Hexagon"

}