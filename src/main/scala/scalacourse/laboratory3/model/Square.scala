package scalacourse.laboratory3.model

class Square(side1: Double, side2: Double, side3: Double, side4: Double, angle: Double)
  extends Quadrangle(side1: Double, side2: Double, side3: Double, side4: Double, angle: Double) {

  override def name(): String = "Square"

}