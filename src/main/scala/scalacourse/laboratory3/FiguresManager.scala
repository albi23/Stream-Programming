package scalacourse.laboratory3

import scalacourse.laboratory3.model._
import scalacourse.utils.NumberUtils

import scala.collection.mutable.ArrayBuffer

object FiguresManager {

  private final val figuresArgs = Map("c" -> 1, "q" -> 5, "p" -> 1, "h" -> 1)

  def handleArguments(args: Array[String]): ArrayBuffer[Figure] = {
    var parsedArgsIndex: Int = 1
    val figures = new ArrayBuffer[Figure]()

    for (figureDef <- args(0).zipWithIndex) {
      val figureRequiredArgs = figuresArgs.get(figureDef._1.toString)
      if (figureRequiredArgs.isEmpty) {
        println("Entered incorrect param " + figureDef)
      } else {
        validateFigureDef(args, figureDef._1.toString, parsedArgsIndex, figureRequiredArgs.get, figures)
        parsedArgsIndex += figureRequiredArgs.get
      }
    }
    figures
  }

  private def validateFigureDef(args: Array[String],
                                figureDef: String,
                                parsedArgsIndex: Int,
                                figureRequiredArgs: Int,
                                resultFigures: ArrayBuffer[Figure]): Unit = {
    val figureArgs = args.slice(parsedArgsIndex, parsedArgsIndex + figureRequiredArgs)
    if (checkUnParsedArguments(figureDef, figureRequiredArgs, figureArgs)) {
      val parsedArgs = figureArgs.map(v => v.toDouble)
      if (checkOrderAndAngle(figureDef, parsedArgs)) {
        resultFigures += figureFactory(figureDef, parsedArgs)
      }
    }
  }

  private def figureFactory(figureIdentifier: String, args: Array[Double]): Figure = {
    figureIdentifier match {
      case "c" => new Circle(args(0))
      case "q" =>
        if (math.min(args(0), args(2)) != math.max(args(1), args(3)) && args(4) == 90)
          return new Rectangle(args(0), args(1), args(2), args(3), args(4))
        if (math.min(args(0), args(2)) == math.max(args(1), args(3)) && args(4) == 90)
          return new Square(args(0), args(1), args(2), args(3), args(4))
        if (math.min(args(0), args(2)) == math.max(args(1), args(3)))
          new Rhombus(args(0), args(1), args(2), args(3), args(4))
        throw new IllegalArgumentException(f"Incorrect arguments ${args.toList} for figure def $figureIdentifier")
      case "p" => new Pentagon(args(0))
      case "h" => new Hexagon(args(0))
    }
  }

  private def checkUnParsedArguments(figureIdentifier: String, requiredArgsCount: Int, args: Array[String]): Boolean = {
    for (arg <- args) {
      if (!NumberUtils.isCorrectNumber(arg, x => x.toDouble)) return false
    }
    if (args.length != requiredArgsCount) {
      println(f"Incorrect args count [${args.length}] for figure [$figureIdentifier]")
      return false
    }
    true
  }

  private def checkOrderAndAngle(figureIdentifier: String, args: Array[Double]): Boolean = {
    if (!figureIdentifier.equals("q")) return true
    //    if (!((args(0) == args(1)) && (args(2) == args(3)))) {
    //      println(f"Incorrect args order for figure def [$figureIdentifier] ${args.mkString("[", ", ", "]")}")
    //      return false
    //    }
    if (!(0 < args(4) && args(4) < 180)) {
      println(f"Incorrect angle value=${args(4)}  for figure def [$figureIdentifier]")
      return false
    }
    true
  }
}
