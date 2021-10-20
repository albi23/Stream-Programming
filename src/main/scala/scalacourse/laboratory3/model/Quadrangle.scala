package scalacourse.laboratory3.model

abstract class Quadrangle(side1: Double, side2: Double, side3: Double, side4: Double, angle: Double) extends Figure {

  override def perimeter(): Double = side1 * side1

  override def fieldArea(): Double = 4 * side1

  override def name(): String = "Quadrangle"

}

