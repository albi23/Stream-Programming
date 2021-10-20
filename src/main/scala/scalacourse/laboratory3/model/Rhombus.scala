package scalacourse.laboratory3.model

class Rhombus(side1: Double, side2: Double, side3: Double, side4: Double, angle: Double) extends
    Quadrangle(side1: Double, side2: Double, side3: Double, side4: Double, angle: Double) {


  override def perimeter(): Double = side1 * side1 * Math.sin(angle)

  override def name(): String = "Rhombus"

}