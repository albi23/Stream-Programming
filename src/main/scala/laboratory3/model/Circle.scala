package laboratory3.model

class Circle(val radius: Double) extends Figure {

    override def perimeter(): Double = math.Pi * radius * radius
    override def fieldArea(): Double = math.Pi * 2 * radius
    override def name(): String = "Circle"

}
