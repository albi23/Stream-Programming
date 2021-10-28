package bigdataalgorithmscourse.labolatory1

import java.io.File
import java.util.regex.Pattern
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using

object WordsImportance {

  case class Book(id: Int, title: String);

  private final val ROOT_DIR: String = System.getProperty("user.dir") + File.separator + String.join(File.separator, "src", "resources") + File.separator
  private final val STOP_WORDS: Set[String] = Using(Source.fromFile(ROOT_DIR + "stopwords_en.txt"))(s => s.getLines().toSet).get;
  private final val SPLIT_REGEX = Pattern.compile("""[^\x21-\x7E]|[\p{Punct}]""")


  def main(args: Array[String]): Unit = {

    val books = List(
      Book(1, "A-Point-of-Testimony.txt"),
      Book(2, "testBooks/The-Declaration-of-Independence-of-the-United-States-of-America.txt"),
      Book(3, "MergedBooks")
    )

    val statisticMap = loadBooksFromFile(books, withMergedValue = true)
    printNTheMostFrequentWords(30, statisticMap, books)
    TFIDF(10, statisticMap, books)
    comparisonBooks(1)
  }

  def comparisonBooks(mode: Int): Unit = {

    val inputBooks = mode match{
      case 1 => Range(1, 11)
        .map(i => Book(i, f"testBooks/The Essays of Montaigne, V$i.txt"))
        .toList
      case 2 =>   Range(1, 6)
        .map(i => Book(i, f"testBooks/The Essays of Montaigne, V$i.txt"))
        .toList.appendedAll(
        List("A Spring Harvest, Tolkien.txt", "Explanation of Terms Used in Entomology.txt",
          "History of the War.txt", "Principles of Orchestration.txt", "Rome and Juliet.txt")
          .zipWithIndex.map(entry => Book(entry._2 + 6, f"testBooks/"+entry._1)));
      case _ => List(
        Book(1, "testBooks/A Spring Harvest, Tolkien.txt"),
        Book(2, "testBooks/Explanation of Terms Used in Entomology.txt"),
        Book(3, "testBooks/History of the War.txt"),
        Book(4, "testBooks/Principles of Orchestration.txt"),
        Book(5, "testBooks/Rome and Juliet.txt"),
        Book(6, "testBooks/Symbolic Logic.txt"),
        Book(7, "testBooks/The Economist.txt"),
        Book(8, "testBooks/The Legends of the Jews.txt"),
        Book(9, "testBooks/The Special and General Theory, Albert Einstein.txt"),
        Book(10, "testBooks/The Whitehouse Cookbook.txt"),
      )
    }

    val statisticMap = loadBooksFromFile(inputBooks)
    printNTheMostFrequentWords(30, statisticMap, inputBooks)
    TFIDF(30, statisticMap, inputBooks)

  }

  def TFIDF(n: Int, statisticMap: mutable.Map[Int, mutable.Map[String, Int]], books: List[Book]): Unit = {
    books.foreach(book => {
      val tfidfResults = ArrayBuffer.empty[(Double, String)]
      val wordOccurrencesInDocument = statisticMap(book.id).values.sum.toDouble
      if (wordOccurrencesInDocument == 0) {
        throw new IllegalStateException("Can not determine TFIDF for empty set.")
      }

      statisticMap(book.id).foreach(entry => {
        val wordInDocuments: Int = books.map(b => if (statisticMap(b.id).contains(entry._1)) 1 else 0).sum
        val result = (entry._2 / wordOccurrencesInDocument) * Math.log(books.size / wordInDocuments.doubleValue)
        tfidfResults.addOne((result, entry._1))
      })
      println(f"Data for: $book \n" + tfidfResults.sortWith(_._1 > _._1).take(n).mkString(" ") + "\n")
    })
  }

  private def printNTheMostFrequentWords(n: Int, statisticMap: mutable.Map[Int, mutable.Map[String, Int]], books: List[Book]): Unit = {
    println("--- Most Frequent Words ---")
    books.foreach(book =>
      println("Data for book: " + book.title + "\n" + ListMap(statisticMap(book.id).toSeq.sortWith(_._2 > _._2): _*).take(n).mkString("\n"))
    )
  }

  private def loadBooksFromFile(books: List[Book], withMergedValue: Boolean = false): mutable.Map[Int, mutable.Map[String, Int]] = {
    val statisticMap: mutable.Map[Int, mutable.Map[String, Int]] = mutable.Map[Int, mutable.Map[String, Int]]()
    var mergedOccurrences: mutable.Map[String, Int] = mutable.Map[String, Int]()
    books.foreach(book => {
      Using(Source.fromFile(ROOT_DIR + book.title)) {
        source => {
          val occurrencesPerBook: mutable.Map[String, Int] = parseInputDataSource(source.getLines())
          if (withMergedValue) {
            mergedOccurrences = merge(mergedOccurrences, occurrencesPerBook)
          }
          statisticMap += book.id -> occurrencesPerBook
        }
      }
    })
    if (withMergedValue) statisticMap += books.size -> mergedOccurrences else statisticMap
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
