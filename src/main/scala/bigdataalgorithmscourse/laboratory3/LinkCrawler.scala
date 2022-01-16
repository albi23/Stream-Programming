package bigdataalgorithmscourse.laboratory3

import bigdataalgorithmscourse.utils.Color.YELLOW
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.select.Elements

import java.util.Objects.hash
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class LinkCrawler(val toVisitUrls: Array[String],
                  var visitedUrls: java.util.Set[String],
                  val hostName: String,
                  val urlConnections: ConcurrentHashMap[String, Set[String]],
                  val definedDeep: Int = 2,
                  val initLatencyThreads: Int = 1500 /*ms*/) {

  private val urlsQueue = getConcurrentLinkedSet
  private val reachedPage: AtomicInteger = new AtomicInteger()
  private val totalLinks: AtomicInteger = new AtomicInteger()
  private val rejectedPages: AtomicInteger = new AtomicInteger()

  case class SingleHashTuple(_1: String, _2: String, _3: Int) {
    override def hashCode(): Int = hash(_1)
  }

  def toRunCrawlingThreads: List[Thread] = {
    toVisitUrls.foreach(url => urlsQueue.addOne(SingleHashTuple(url, url, 0)))

    val threads = Iterator.from(1)
      .takeWhile(x => x <= Runtime.getRuntime.availableProcessors())
      .map(id => {
        val th = new Thread(() => {
          if (id != 1) {
            Thread.sleep(initLatencyThreads)
          }
          println(YELLOW.makeColor("[Info] " + Thread.currentThread().getName + "  START crawl"))
          crawl(definedDeep)
          println(YELLOW.makeColor("[Info] " + Thread.currentThread().getName + "  END crawl"))
        })
        th.setName(s"[Crawler $id]")
        th
      }).toList

    threads
  }

  private def crawl(deep: Long = 5): Unit = {

    while (urlsQueue.nonEmpty) {
      val url = urlsQueue.head._1
      val tuple = urlsQueue.head
      urlsQueue.remove(tuple)
      val optDoc: Option[Document] = request(url)
      if (optDoc.isDefined) {
        reachedPage.incrementAndGet()

        visitedUrls.add(url)
        val links: Elements = optDoc.get.select("a[href]")
        val siblings = ListBuffer[String]()
        links.forEach(link => {
          val nextLink = link.absUrl("href").split("#")(0) // remove redirection to some part of document
          val bool = tuple._3 < deep && nextLink != url && nextLink.contains(hostName) &&
            !nextLink.contains("Template:")
          if (bool) {
            totalLinks.incrementAndGet()
            if (!this.visitedUrls.contains(nextLink)) {
              siblings.addOne(nextLink)
              val newOne = SingleHashTuple(nextLink, url, tuple._3 + 1)
              urlsQueue.addOne(newOne)
            }

          }
        })
        if (siblings.isEmpty || optDoc.isEmpty) {
          siblings.addOne(tuple._2)
        }
        urlConnections.put(url, siblings.toSet)
      } else {
        rejectedPages.incrementAndGet()
      }
    }
  }

  def currentQueueSize(): Int = urlsQueue.size

  def extendedInfo(): Unit = {
    println(YELLOW.makeColor(s"\n[Info] ### Not reachable pages ${rejectedPages.get()}"))
    println(YELLOW.makeColor(s"[Info] ### Reached pages ${reachedPage.get()}"))
    println(YELLOW.makeColor(s"[Info] ### Avg links amount on pages ${totalLinks.get() / reachedPage.get().toDouble}"))
    println(YELLOW.makeColor(s"[Info] Connection between pages:"))
//    val v = urlConnections.get("https://en.wikipedia.org/wiki/Main_Page")
//          print("https://en.wikipedia.org/wiki/Main_Page with => " + v.map(l => l.split("org")(1)).mkString("[", ", ", "]") + "\n\n")

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


  private def getConcurrentLinkedSet = {
    new mutable.LinkedHashSet[SingleHashTuple]() {

      override def addOne(e: SingleHashTuple): this.type = synchronized {
        val value = super.addOne(e)
        value
      }

      override def remove(elem: SingleHashTuple): Boolean = synchronized {
        super.remove(elem)
      }
    }
  };


}