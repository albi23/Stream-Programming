package bigdataalgorithmscourse.laboratory3

import bigdataalgorithmscourse.utils.Color.{BLUE, RED, YELLOW}
import org.jsoup.Jsoup
import org.jsoup.nodes.Document

import java.io.{File, FileWriter}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.{BufferedSource, Source}
import scala.util.Using


object SearchEngine {

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println(s"${RED.makeColor("Usage page_ranking_absolute_path.txt searchedWord")}")
      System.exit(1)
    }
    val openedSource = Using(Source.fromFile(args(0)))(source => collectDataFromFile(source))
    if (openedSource.isFailure) {
      openedSource.failed.get.printStackTrace()
      System.exit(1)
    } else {
      val currentPageRank: ListBuffer[(String, Double)] = openedSource.get

      val searchedWord = args(1)
      println(YELLOW.makeColor(s"[Info] Searched word: $searchedWord"))


      val progress = new AtomicInteger(1)
      val futureResult: Future[ListBuffer[(String, Double)]] =
        Future.traverse(currentPageRank) { pageRank =>
          Future {
            var updatedPageRank: (String, Double) = null
            val optDoc: Option[Document] = request(pageRank._1)
            if (optDoc.isDefined) {
              val wordCount = optDoc.get.text().split(" ").count(x => searchedWord.equalsIgnoreCase(x))
              updatedPageRank = (pageRank._1, pageRank._2 * wordCount)
              print(YELLOW.makeColor(s"\r ${(progress.incrementAndGet().toDouble / currentPageRank.length) * 100}% [word count]: $wordCount [Page] ${pageRank._1} "))
            } else {
              updatedPageRank = (pageRank._1, 0)
            }
            updatedPageRank
          }
        }

      val result = Await.result(futureResult, Duration.Inf) // waiting for threads
      println("\r\n\n")
      val res = result
        .sortBy { case (_, x) => x * (-1) }
        .take(5)
        .map(x => f"${BLUE.makeColor("[Value]")}%s ${x._2}%5.18f ${BLUE.makeColor("[Page]")}%s: ${x._1}%s ")
      res
        .foreach(rank => println(rank))
      Using(new FileWriter(System.getProperty("user.dir") + File.separator + s"rank_for_word_${searchedWord}.txt")) { fileWriter => {
        res.foreach(entry => {fileWriter.write(entry + "\n")})
      }
      }
    }
  }

  def request(url: String): Option[Document] = {
    try {
      val connection = Jsoup.connect(url)
      val doc = connection.get()
      if (connection.response().statusCode() == 200) {
        return Option(doc)
      }
    } catch {
      case ex: Throwable =>
        System.err.println(s"[Thread: ${Thread.currentThread().getName}][Error  for url]: $url  [${ex.getClass.getSimpleName}] Cause " + ex.getMessage)
    }
    Option.empty
  }

  private def collectDataFromFile(source: BufferedSource): ListBuffer[(String, Double)] = {
    val rank = ListBuffer[(String, Double)]()
    for (line <- source.getLines()) {
      val pair = line.split("\\|").map(s => s.trim)
      rank.addOne(pair(0), pair(1).toDouble)
    }
    rank
  }
}
