package bigdataalgorithmscourse.labolatory2

import java.io.File
import java.util.regex.Pattern
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using

object GraphAnalysis {

  private final val ROOT_DIR: String = System.getProperty("user.dir") + File.separator + String.join(File.separator, "src", "resources") + File.separator
  private final val GRAPH_DEF_FILE: String = "web-Stanford.txt"
  private final val SPLIT_REGEX = Pattern.compile("""\s""")


  def main(args: Array[String]): Unit = {
    val triedUnit = Using(Source.fromFile(ROOT_DIR + GRAPH_DEF_FILE)) { source => {
      source.getLines()
        .filter(line => !line.startsWith("#"))
        .map(line => mapToEdge(line))
        .foldLeft(mutable.Map[Int, (Int, Int)]()) { (collector, edgeDef) => reduceGraphEdges(edgeDef, collector)}
    }
    }
    if (triedUnit.isFailure) {
      triedUnit.failed.get.printStackTrace()
      System.exit(1)
    }
    val resultMap = triedUnit.get
    println(resultMap.map(entry => "(" + entry._1 + ", inDeg(" + entry._1 + ") = " + entry._2._1 + ", outDeg(" + entry._1 + ") = " + entry._2._2 + ")").mkString(",\n"))
    println(resultMap(2)) // just to check for vertex "2"
  }

  def mapToEdge(line: String): (Int, Int) = {
    val words: Array[String] = SPLIT_REGEX.split(line)
    var parsedNumber: Int = 0
    val edgeDef = new ArrayBuffer[Int](2)
    for (word <- words) {
      try {
        val asInt = word.toInt
        parsedNumber += 1
        if (parsedNumber <= 2) {
          edgeDef.addOne(asInt)
        } else {
          throw new IllegalArgumentException(" Line \"" + line + "\" contains incorrect input")
        }
      } catch {
        case ex: NumberFormatException => throw new IllegalArgumentException(" Line \"" + line + "\" contains incorrect input bla bla")
        case _: Throwable => throw new RuntimeException();
      }
    }
    (edgeDef(0), edgeDef(1))
  }

  def reduceGraphEdges(edgeDef: (Int, Int), collector: mutable.Map[Int, (Int, Int)]): mutable.Map[Int, (Int, Int)] = {
    val inDegUpdate = collector.getOrElse(edgeDef._2, (0, 0))
    val outDegUpdate = collector.getOrElse(edgeDef._1, (0, 0))
    collector(edgeDef._2) = (inDegUpdate._1 + 1, inDegUpdate._2)
    collector(edgeDef._1) = (outDegUpdate._1, outDegUpdate._2 + 1)
    collector
  }

}
