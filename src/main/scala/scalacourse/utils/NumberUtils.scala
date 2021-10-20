package scalacourse.utils

object NumberUtils {

  def isCorrectNumber[T](strArg: String, f: String => T)(implicit n: Numeric[T]): Boolean = {
    try {
      val value = f(strArg)
      if (value.asInstanceOf[Double] < 0) throw new IllegalArgumentException
      return true
    } catch {
      case _: NumberFormatException => println("Invalid argument \'" + strArg + "\'");
      case _: IllegalArgumentException => println("Out of range number " + strArg);
    }
    false
  }

}
