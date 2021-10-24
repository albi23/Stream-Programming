package bigdataalgorithmscourse.labolatory1

import bigdataalgorithmscourse.labolatory1.WordsImportance.Book.{BOOK1, BOOK2, MERGED}

import java.io.File
import java.util.regex.Pattern
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using

object WordsImportance {

  object Book extends Enumeration {
    type Book = Value

    val BOOK1: Book = Value("A-Point-of-Testimony.txt")
    val BOOK2: Book = Value("The-Declaration-of-Independence-of-the-United-States-of-America.txt")
    val MERGED: Book = Value("Merged_Books")
  }

  private final val BOOKS = List(BOOK1, BOOK2)
  private final var GENERATOR_CONTEXT: mutable.Map[Int, mutable.Map[String, Int]] = mutable.Map[Int, mutable.Map[String, Int]]()
  private final val ROOT_DIR: String = System.getProperty("user.dir") + File.separator + String.join(File.separator, "src", "resources") + File.separator
  private final val STOP_WORDS: Set[String] = Using(Source.fromFile(ROOT_DIR + "stopwords_en.txt"))(s => s.getLines().toSet).get;
  private final val SPLIT_REGEX = Pattern.compile("""[^\x21-\x7E]|[\p{Punct}]""")


  def main(args: Array[String]): Unit = {
    loadFiles
    printNTheMostFrequentWords(30)
    TFIDF()
  }

  def TFIDF(): Unit = {
    BOOKS.foreach(book => {
      val tfidfResults = ArrayBuffer.empty[(Double, String)]
      println("Data for: " + book)
      val wordOccurrencesInDocument = GENERATOR_CONTEXT(book.id).values.sum.toDouble
      if (wordOccurrencesInDocument == 0) {
        throw new IllegalStateException("Can not determine TFIDF for empty set.")
      }

      GENERATOR_CONTEXT(book.id).foreach(entry => {
        val wordInDocuments: Int = BOOKS.map(b => if (GENERATOR_CONTEXT(b.id).contains(entry._1)) 1 else 0).sum
        val result = (entry._2 / wordOccurrencesInDocument) * Math.log(BOOKS.size / wordInDocuments.doubleValue)
        tfidfResults.addOne((result, entry._1))
      })

      println(tfidfResults.sortWith(_._1 > _._1).take(10))
    })
  }

  private def printNTheMostFrequentWords(n: Int): Unit = {
    Book.values.foreach(book => {
      println("Data for book: " + book)
      val mostCommonWords = ListMap(GENERATOR_CONTEXT(book.id).toSeq.sortWith(_._2 > _._2): _*).take(n).mkString("\n")
      println(mostCommonWords)
    })
  }

  private def loadFiles: mutable.Map[Int, mutable.Map[String, Int]] = {
    var mergedOccurrences: mutable.Map[String, Int] = mutable.Map[String, Int]()
    BOOKS.foreach(book => {
      Using(Source.fromFile(ROOT_DIR + book)) {
        source => {
          val occurrencesPerBook: mutable.Map[String, Int] = parseInputDataSource(source.getLines())
          mergedOccurrences = merge(mergedOccurrences, occurrencesPerBook)
          GENERATOR_CONTEXT += book.id -> occurrencesPerBook
        }
      }
    })
    GENERATOR_CONTEXT += MERGED.id -> mergedOccurrences
  }

  private def parseInputDataSource(source: Iterator[String]): mutable.Map[String, Int] = {
    source
      .filter(line => !line.isBlank)
      .flatMap(line => SPLIT_REGEX.split(line).map(word => word.strip()).filter(word => word.nonEmpty && !STOP_WORDS.contains(word.toLowerCase)))
      .foldLeft(mutable.Map.empty[String, Int]) { (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) }
  }

  def merge(collectorMap: mutable.Map[String, Int], partialMap: mutable.Map[String, Int]): mutable.Map[String, Int] = {
    val tmp = collectorMap ++ partialMap.map { case (k, v) => k -> (v + collectorMap.getOrElse(k, 0)) }
    tmp
  }

}
