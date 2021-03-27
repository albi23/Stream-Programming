package laboratory3

object FiguresMain {

  def main(args: Array[String]): Unit = {

    val testArg = Array[String]("abcdcccppphhhqqqq", "1","0","-1","0","2", "-2", "-5", "0", "5", "2",
      "1", "1", "2", "2", "50",
      "1", "2", "3", "4", "90",
      "1", "1", "1", "1", "90",
      "1", "1", "1", "1", "190",
      "1", "1", "1", "1", "0",
      "2", "2", "3", "4", "90",
      "4", "4", "6", "6", "90")
    if (!testArg.isEmpty) {
      FiguresManager.handleArguments(testArg)
        .foreach(figure => println(f"${figure.name()} Area=${figure.fieldArea()}  perimeter=${figure.perimeter()}"))
    } else {
      println("No arguments")
      System.exit(1)
    }
  }
}
