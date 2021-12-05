package bigdataalgorithmscourse.labolatory2

import bigdataalgorithmscourse.utils.{Color}

import java.io.File
import java.util.regex.Pattern
import scala.collection.mutable
import scala.io.Source
import scala.util.Using


object DocumentSimilarity {

  private final val ROOT_DIR: String = System.getProperty("user.dir") + File.separator + String.join(File.separator, "src", "resources") + File.separator
  private final val STOP_WORDS: Set[String] = Using(Source.fromFile(ROOT_DIR + "stopwords_en.txt"))(s => s.getLines().toSet).get;
  private final val SPLIT_REGEX = Pattern.compile("""[^\x21-\x7E]|[\p{Punct}]""")


  def main(args: Array[String]): Unit = {
    documentSimilarityTest()
  }

  def documentSimilarityTest(): Unit = {
    val kParams = List.range(2, 14)
    val testBooks = List("The Essays of Montaigne, V1.txt", "The Essays of Montaigne, V2.txt",
      "The Essays of Montaigne, V3.txt", "Rome and Juliet.txt",
      "The Whitehouse Cookbook.txt", "The Economist.txt")

    val bookOnWords: List[(String, List[String])] = testBooks.map(testBook => {
      val bookDir = ROOT_DIR + "testBooks" + File.separator + testBook
      val parsedWordSet = Using(Source.fromFile(bookDir))(source => parseInputDataSource(source.getLines())).get
      (testBook, parsedWordSet)
    })

    kParams.foreach(paramK => {
      println(Color.RED.makeColorBg(s"[For K $paramK]"))
      bookOnWords.foreach(bookWordTuple =>{
        println(Color.BLUE.makeColor("--> For book: "+bookWordTuple._1 + " <--"))
        val shinglesA = constructShingles(paramK, bookWordTuple._2 )
        val str = bookOnWords.filter(tuple => !tuple._1.equals(bookWordTuple._1))
          .map(pairBook => {
            val shinglesB = constructShingles(paramK, pairBook._2)
            (pairBook._1, jaccardSimilarity(shinglesA, shinglesB))
          }).mkString(",\n")
        println(str+" \n")

      })
    })


  }

  private def jaccardSimilarity[T](set1: Set[T], set2: Set[T]): Double = {
    set1.intersect(set2).size.toDouble / set1.union(set2).size.toDouble
  }

  private def parseInputDataSource(source: Iterator[String]): List[String] = {
    source
      .filter(line => !line.isBlank)
      .flatMap(line => {
        SPLIT_REGEX.split(line)
          .map(word => word.strip().toLowerCase)
          .filter(word => word.nonEmpty && !STOP_WORDS.contains(word))
      }).toList
  }


  private def constructShingles(k: Int, iterator: List[String]): Set[String] = {
    var shingles = Set[String]()
    val stack = mutable.Stack.from(iterator.take(k))
    shingles += stack.mkString(" ")

    iterator.foreach(word => {
      stack.pop()
      stack.append(word)
      shingles += stack.mkString(" ")
    })

    shingles
  }
}
