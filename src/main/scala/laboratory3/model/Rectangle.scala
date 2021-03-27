package laboratory3.model

class Rectangle(side1: Double, side2: Double, side3: Double, side4: Double, angle: Double) extends
  Quadrangle(side1: Double, side2: Double, side3: Double, side4: Double, angle: Double) {

  override def perimeter(): Double = side2 * side3

  override def fieldArea(): Double = 2 * (side2 +  side3)

  override def name(): String = "Rectangle"

}