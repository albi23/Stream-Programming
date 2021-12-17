package bigdataalgorithmscourse.labolatory2

import bigdataalgorithmscourse.utils.Color

import java.io.{File, FileWriter}
import java.util.regex.Pattern
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Random, Using}


object DocumentSimilarity {

  private final val ROOT_DIR: String = System.getProperty("user.dir") + File.separator + String.join(File.separator, "src", "resources") + File.separator
  private final val STOP_WORDS: Set[String] = Using(Source.fromFile(ROOT_DIR + "stopwords_en.txt"))(s => s.getLines().toSet).get;
  private final val SPLIT_REGEX = Pattern.compile("""[^\x21-\x7E]|[\p{Punct}]""")


  def main(args: Array[String]): Unit = {
    intersectionSimilarityTest()
    minhashSignaturesSimilarityTest()
  }

  def intersectionSimilarityTest(): Unit = {
    val tuple = getTestData()
    intersectionSimilarity(tuple._1, tuple._2)

  }

  def minhashSignaturesSimilarityTest(): Unit = {
    val tuple = getTestData()
    minhashSignaturesSimilarity(tuple._1, tuple._2)
  }

  def getTestData(): (List[Int], List[(String, List[String])]) = {
    val kParams = List.range(2, 14)
    val testBooks = List("The Essays of Montaigne, V1.txt", "The Essays of Montaigne, V2.txt",
      "The Essays of Montaigne, V3.txt", "Rome and Juliet.txt",
      "The Whitehouse Cookbook.txt", "The Economist.txt")

    val bookOnWords: List[(String, List[String])] = testBooks.map(testBook => {
      val bookDir = ROOT_DIR + "testBooks" + File.separator + testBook
      val parsedWordSet = Using(Source.fromFile(bookDir))(source => parseInputDataSource(source.getLines())).get
      (testBook, parsedWordSet)
    })

    (kParams, bookOnWords)
  }

  private def minhashSignaturesSimilarity(kParams: List[Int], bookOnWords: List[(String, List[String])]): Unit = {

    val outDir = System.getProperty("user.dir") + "/src/main/scala/bigdataalgorithmscourse/labolatory2/minhashresults.txt".replaceAll("//", File.separator)
    Using(new FileWriter(outDir)) { fileWriter => {
      val booksTitles: List[String] = bookOnWords.map(x => x._1)
      val hashFunctionsParams: List[Int] = List(10, 100, 250, 500)
      kParams.foreach(paramK => {
        println("k = " + paramK)
        val shinglesArr = new ArrayBuffer[Set[String]](bookOnWords.size)
        var allWordsCollection = Set[String]()
        bookOnWords.foreach(bookWordTuple => {
          val shingles = constructShingles(paramK, bookWordTuple._2)
          shinglesArr += shingles
          allWordsCollection = allWordsCollection.union(shingles)
        })

        val allWords = allWordsCollection.toArray
        hashFunctionsParams.foreach(hashFunctionCount => {
          val signatures: Array[Array[Int]] = calculateSignatures(allWords, shinglesArr, hashFunctionCount)
          fileWriter.append(getPrintResults(paramK, hashFunctionCount, booksTitles, signatures))
        })

      })
    }
    }
  }

  def getPrintResults(kParam: Int, hashFunctionCount: Int, books: List[String], signatures: Array[Array[Int]]): String = {

    var result: mutable.StringBuilder = new StringBuilder();
    for ((bookTitle, idx1) <- books.view.zipWithIndex) {
      result ++= s"\n[For K $kParam] [For n=$hashFunctionCount] --> For book: $bookTitle  <--\n"
      val sigBookA = signatures(idx1).toSet
      for ((pariBook, idx2) <- books.view.zipWithIndex) {
        if (idx1 != idx2) {
          result ++= s"($pariBook, ${jacquardSimilarity(sigBookA, signatures(idx2).toSet)}),\n"
        }
      }
    }
    result.toString()
  }

  private def calculateSignatures(allWords: Array[String], shinglesArr: ArrayBuffer[Set[String]], hashingFunctionCount: Int): Array[Array[Int]] = {
    val hashValues: Array[Int] = List.range(0, allWords.length).toArray
    val hashTable: Array[mutable.ArraySeq[Int]] = Iterator.from(0)
      .takeWhile(i => i < hashingFunctionCount)
      .map(_ => Random.shuffle(hashValues)).toArray

    val signaturesArr = new Array[Array[Int]](shinglesArr.length)
    val zipWithIndex = allWords.view.zipWithIndex
    for ((bookShingles, bookIdx) <- shinglesArr.view.zipWithIndex) {
      val singleColSignature = new Array[Int](allWords.length)
      for ((word, idx) <- zipWithIndex) {
        if (bookShingles.contains(word)) {
          singleColSignature(idx) = Iterator.range(0, hashingFunctionCount).map(i => hashTable(i)(idx)).min
        } else {
          singleColSignature(idx) = Int.MaxValue
        }
      }
      signaturesArr(bookIdx) = singleColSignature
    }

    signaturesArr
  }

  private def intersectionSimilarity(kParams: List[Int], bookOnWords: List[(String, List[String])]): Unit = {
    kParams.foreach(paramK => {
      println(Color.RED.makeColorBg(s"[For K $paramK]"))
      for (i <- 0 until  bookOnWords.size - 1){
        println(Color.BLUE.makeColor("--> For book: " + bookOnWords(i)._1 + " <--"))
        val shinglesA: Set[String] = constructShingles(paramK, bookOnWords(i)._2)
        for (j <- (i + 1) until bookOnWords.size){
          val shinglesB: Set[String] = constructShingles(paramK, bookOnWords(j)._2)
          println((bookOnWords(j)._1, jacquardSimilarity(shinglesA, shinglesB)))
        }
        println(" \n")
      }
    })
  }

  private def jacquardSimilarity[T](set1: Set[T], set2: Set[T]): Double = {
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
