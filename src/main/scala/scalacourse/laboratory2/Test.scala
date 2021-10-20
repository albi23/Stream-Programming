package scalacourse.laboratory2

object Test {

  def main(args: Array[String]): Unit = {

    if (args.isEmpty) {
      println("No arguments.")
      System.exit(1)
    } else if (!isCorrectNumber(args(0))) {
      System.exit(1)
    }

    //        extractValueFromRange(new PrimeNumbers(args(0).toInt), args)
    extractValueFromRange(new PascalTriangleRow(args(0).toInt), args)
  }


  private def extractValueFromRange(valueHolder: ValueHolder, args: Array[String]): Unit = {

    for (i <- 1 until args.length) {
      if (isCorrectNumber(args(i))) {
        val intVal = args(i).toInt
        println(if (valueHolder.containsIndex(intVal)) intVal + " - " + valueHolder.valueAt(intVal) else "out of range number " + intVal)
      }
    }
  }

  def isCorrectNumber(strArg: String): Boolean = {
    try {
      val int = strArg.toInt
      if (int < 0) throw new IllegalArgumentException
      return true
    } catch {
      case _: NumberFormatException => println("invalid argument \'" + strArg + "\'");
      case _: IllegalArgumentException => println("out of range number " + strArg);
    }
    false
  }
}
