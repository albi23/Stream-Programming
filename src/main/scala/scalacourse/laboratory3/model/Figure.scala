package scalacourse.laboratory3.model

abstract class Figure {

  def perimeter(): Double
  def fieldArea(): Double
  def name(): String = "AbstractFigure"
}
