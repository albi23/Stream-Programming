package bigdataalgorithmscourse.laboratory0

import java.io.{File, FileWriter}
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.io.{Source, StdIn}
import scala.util.Using

object WordCloudGenerator {

  private final var GENERATOR_CONTEXT: mutable.Map[String, Int] = collection.mutable.Map[String, Int]()
  private final val ROOT_DIR: String = System.getProperty("user.dir") + File.separator + String.join(File.separator, "src", "resources") + File.separator
  private final val WC_OUT_DIR: String = ROOT_DIR + "wc.csv";
  private final val STOP_WORDS: Set[String] = Using(Source.fromFile(ROOT_DIR + "stopwords_en.txt"))(s => s.getLines().toSet).get;
  private final val COMMANDS: String = "Select Option\n1) Give pure string\n2) Give file\n3) Print selected number of " +
    "most frequent words\n4) Write selected number of most frequent words to csv file\n5) Exit\n";

  def main(args: Array[String]): Unit = {

    while (true) {
      println(COMMANDS);
      StdIn.readLine() match {
        case "1" =>
          print("Enter input text:")
          merge(parseInputDataSource(List(StdIn.readLine()).iterator))
        case "2" =>
          print("Enter path to string file:")
          Using(Source.fromFile(StdIn.readLine())) { source => merge(parseInputDataSource(source.getLines())) }
        case "3" =>
          print("How many of word cloud print?:")
          println(ListMap(GENERATOR_CONTEXT.toSeq.sortWith(_._2 > _._2): _*).take(StdIn.readLine().toInt).toString()+"\n")
        case "4" =>
          print("How many of word cloud save in *.csv file?:")
          val str = ListMap(GENERATOR_CONTEXT.toSeq.sortWith(_._2 > _._2): _*).take(StdIn.readLine().toInt)
            .map(entry => entry._1 + ", " + entry._2 + "\n")
            .mkString("")
          Using(new FileWriter(WC_OUT_DIR)) { fileWriter => fileWriter.write(str); fileWriter.flush() }
        case "5" => System.exit(0)
        case _ =>
          println("Wrong input")
      }
    }

  }

  private def parseInputDataSource(source: Iterator[String]): Map[String, Int] = {
    source
      .filter(line => !line.isBlank)
      .flatMap(line => line.split("\\p{Punct}| ").map(word => word.strip()).filter(word => !word.isBlank && !STOP_WORDS.contains(word)))
      .foldLeft(Map.empty[String, Int]) { (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) }
  }

  def merge(partialMap: Map[String, Int]): Unit =
    GENERATOR_CONTEXT = GENERATOR_CONTEXT ++ partialMap.map { case (k, v) => k -> (v + GENERATOR_CONTEXT.getOrElse(k, 0)) }

}
