package bigdataalgorithmscourse.laboratory0

import java.io.{File, FileWriter}
import scala.collection.immutable.ListMap
import scala.io.Source
import scala.util.Using

object WordCloudBook {

  private final val ROOT_DIR: String = System.getProperty("user.dir") + File.separator + "/src/main/scala/bigdataalgorithmscourse/laboratory0/"
  private final val BOOK_FILE_PATH: String = ROOT_DIR + "The-Declaration-of-Independence-of-the-United-States-of-America.txt";
  private final val WC_OUT_DIR: String = ROOT_DIR + "wc.txt";

  def main(args: Array[String]): Unit = {

    Using(Source.fromFile(BOOK_FILE_PATH)) { source => {
      val wordOnCount = source.getLines
        .filter(line => !line.isBlank)
        .flatMap(line => line.split("\\p{Punct}| ").map(word => word.strip()).filter(word => !word.isBlank))
        .foldLeft(Map.empty[String, Int]) { (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) }
      val mostCommonWords = ListMap(wordOnCount.toSeq.sortWith(_._2 > _._2): _*).take(100).keys.mkString(" ")
      Using(new FileWriter(WC_OUT_DIR)) {
        fileWriter => fileWriter.write(mostCommonWords)
      }

    }
    }
  }

}
