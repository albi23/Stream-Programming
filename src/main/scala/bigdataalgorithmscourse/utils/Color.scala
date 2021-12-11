package bigdataalgorithmscourse.utils

object Color extends Enumeration {

  protected final case class Val(escapeBg: String, escapeFg: String) extends super.Val {
    def makeColorBg(data: String): String = "\u001b[1m" + escapeBg + data + "\u001b[0m"
    def makeColor(data: String): String = "\u001b[1m" + escapeFg + data + "\u001b[0m"
  }

  val RED:    Val = Val("\u001b[48;5;160m", "\u001b[31m")
  val BLUE:   Val = Val("\u001b[48;5;20m", "\u001b[34m")
  val GREEN:  Val = Val("\u001b[48;5;28m", "\u001b[32m")
  val YELLOW: Val = Val("\u001b[48;5;226m", "\u001b[33m")
  val WHITE:  Val = Val("\u001b[48;5;0m", "\u001b[0m")
  val PINK:   Val = Val("\u001b[48;5;165m", "\u001b[35m")
}
