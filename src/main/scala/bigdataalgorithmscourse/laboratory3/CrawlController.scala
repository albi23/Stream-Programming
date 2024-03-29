package bigdataalgorithmscourse.laboratory3

import bigdataalgorithmscourse.utils.Color.{BLUE, GREEN, RED, YELLOW}
import breeze.linalg.{CSCMatrix, DenseVector}

import java.io.{File, FileWriter}
import java.nio.file.Paths
import java.sql.Timestamp
import java.util.concurrent.ConcurrentHashMap
import java.util.{Date, Scanner}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}
import scala.util.Using
import scala.util.control.Breaks._

object CrawlController {

  private val visitedURLS = createConcurrentSet[String]()
  private val linksConnections = new ConcurrentHashMap[String, Set[String]]()

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println(
        s"""
           |${RED.makeColor("Missing mode argument. Usage")} ${BLUE.makeColor("mode")} <${GREEN.makeColor("args")}> :
           | -  ${BLUE.makeColor("rank")} ${GREEN.makeColor("matrix-multiplication [Int]")} ${GREEN.makeColor("vector-value [Double]")} ${GREEN.makeColor("in-file-name.txt")}
           | -  ${BLUE.makeColor("crawl")} ${GREEN.makeColor("page-count [Int] out-file-name.txt")}
           | -  ${BLUE.makeColor("analyse")}
           |""".stripMargin)
      System.exit(1)
    }

    args(0).toLowerCase match {
      case "rank" => handleRankMode(args)
      case "crawl" => handleCrawlMode(args)
      case "analyse" => handleCrawlMode(args, infoMode = true)
      case _ => throw new IllegalArgumentException("Incorrect input")
    }
  }

  def handleRankMode(args: Array[String]): Unit = {
    if (args.length < 4 || args(2).isBlank) throw new IllegalArgumentException("Incorrect input")

    val allLinks = new mutable.HashMap[String, Int]()
    val pageOnLinks = new mutable.HashMap[String, List[String]]()
    println(YELLOW.makeColor(s"[Info] Reading input file: ${args(3)}"))
    val openedSource = Using(Source.fromFile(args(3)))(source => collectDataFromFile(allLinks, pageOnLinks, source))
    if (openedSource.isFailure) {
      openedSource.failed.get.printStackTrace()
      System.exit(1)
    }

    println(YELLOW.makeColor(s"[Info] Start creating matrix [${allLinks.size} x ${allLinks.size}]"))
    val linksMatrix: CSCMatrix[Double] = createLinksMatrix(allLinks, pageOnLinks)
    var rankVector = DenseVector.fill(allLinks.size, args(2).toDouble)
    val int = args(1).toInt
    print("Matrix multiplication: ")
    for (i <- 1 to int) {
      print(s"\r(${(i.toDouble / int) * 100}%)")
      rankVector = linksMatrix * rankVector
    }

    println(YELLOW.makeColor(s"\r[Info] ### Page Ranking ###"))
    val pageRank = constructResultPageRanking(allLinks, rankVector).sortBy { case (x, _) => -x }
    val resultRank = pageRank.take(100)
    println(resultRank.map(x => f"${BLUE.makeColor("[Value]")}%s ${x._1}%5.18f ${BLUE.makeColor("[Page]")}%s: ${x._2}%s ").mkString("\r", "\n", ""))
    Using(new FileWriter(System.getProperty("user.dir") + File.separator + "big_test_rank.txt")) { fileWriter => {
      pageRank.foreach(entry => {
        fileWriter.write(entry._2 + " | " + entry._1 + "\n")
      })
    }
    }
  }

  private def constructResultPageRanking(allLinks: mutable.HashMap[String, Int], rankVector: DenseVector[Double]): ListBuffer[(Double, String)] = {
    val rankOnLink = new ListBuffer[(Double, String)]
    val vectorData = rankVector.data
    var it: Int = 0;
    for (link <- allLinks) {
      rankOnLink.addOne((vectorData(it), link._1))
      it += 1
    }
    rankOnLink
  }

  private def createLinksMatrix(allLinks: mutable.HashMap[String, Int], pageOnLinks: mutable.HashMap[String, List[String]]) = {
    val sparseMatrix = CSCMatrix.zeros[Double](allLinks.size, allLinks.size)
    var rowIdx: Int = 0;
    val matrixSize = pageOnLinks.size
    for (mapEntry <- pageOnLinks) {
      val redirectionCount = mapEntry._2.length
      mapEntry._2.foreach(url => {
        val colIdx = allLinks.get(url)
        if (colIdx.isDefined) {
          sparseMatrix.update(colIdx.get, rowIdx, 1.0 / redirectionCount)
        }
      })
      rowIdx += 1
      print(s"\r(${(rowIdx.toDouble / matrixSize) * 100}%)")
    }
    sparseMatrix
  }

  private def collectDataFromFile(allLinks: mutable.HashMap[String, Int], pageOnLinks: mutable.HashMap[String, List[String]], source: BufferedSource): Unit = {
    var cnt: Int = 0
    for (line <- source.getLines()) {
      val i = line.indexOf("|")
      val page = line.substring(0, i - 1)
      val pageLinks = line.substring(i + 1).split(", ").map(s => s.trim).toList
      if (!allLinks.contains(page)) {
        allLinks.put(page, cnt);
        cnt += 1
      }
      for (link <- pageLinks) {
        if (!allLinks.contains(link)) {
          allLinks.put(link, cnt)
          cnt += 1
        }
      }
      pageOnLinks.put(page, pageLinks)
    }
  }

  private def handleCrawlMode(args: Array[String], infoMode: Boolean = false) = {
    if (args.length < 3 || args(2).isBlank) throw new IllegalAccessException("Incorrect input")

    val urls: Array[String] = redInputLinks(args(1).toInt)
    val linkCrawler = new LinkCrawler(urls, visitedURLS, "en.wikipedia.org", linksConnections, definedDeep = 2)
    val threads = linkCrawler.toRunCrawlingThreads
    threads.foreach(th => th.start())
    val checker: Thread = getControlThread(linkCrawler, "Checker", extendedInfo = infoMode)
    checker.start()
    threads.foreach(th => th.join())
    checker.join()
    Using(new FileWriter(System.getProperty("user.dir") + File.separator + args(2))) { fileWriter => {
      linksConnections.forEach((k, v) => {
        fileWriter.write(k + " | " + v.mkString(", ") + "\n")
      })
      println(YELLOW.makeColor(s"[Info] Result file saved in: ${
        Paths.get(System.getProperty("user.dir")
          + File.separator + args(2)).toAbsolutePath.normalize
      } "))
    }
    }
  }

  private def getControlThread[T <: LinkCrawler](linkCrawler: T, name: String, infoTimer: Int = 4000, extendedInfo: Boolean = false) = {
    val f = java.text.NumberFormat.getIntegerInstance(new java.util.Locale("pl", "PL"))
    val checker = new Thread(() => {
      breakable {
        while (true) {
          try {
            Thread.sleep(infoTimer);
            println(s"\n[${Thread.currentThread().getName}] Visited links: ${GREEN.makeColor(f.format(visitedURLS.size()))}")
          } catch {
            case ex: InterruptedException =>
              System.err.println("[Checker interrupted]\n" + ex.printStackTrace())
          }
          println(s"[${BLUE.makeColor(new Timestamp(new Date().getTime).toString)}] On Queue links: ${GREEN.makeColor(f.format(linkCrawler.currentQueueSize()))}")
          if (extendedInfo) {
            linkCrawler.extendedInfo()
          }
          if (linkCrawler.currentQueueSize() == 0) {
            break
          }
        }
      }
    })
    println(YELLOW.makeColor(s"[Info] Init control Thread: [$name] Refresh time: [$infoTimer ms]"))
    checker.setName(name)
    checker
  }

  // https://en.wikipedia.org/wiki/Main_Page
  private def redInputLinks(count: Int = -1): Array[String] = {
    println(s"Enter ${if (count > 0) count else ""} links: ")
    val scanner = new Scanner(System.in)
    val strings = scanner.nextLine().split("[\t ,]")
    scanner.close()
    if (count > 0)
      return strings.take(count)
    strings
  }

  private def createConcurrentSet[T]() = java.util.Collections.newSetFromMap(
    new java.util.concurrent.ConcurrentHashMap[T, java.lang.Boolean])

}


