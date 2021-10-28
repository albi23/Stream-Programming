package bigdataalgorithmscourse.labolatory1

import scala.collection.mutable

object MapReduceEx {

  def main(args: Array[String]): Unit = {

    // Let's assume that V = {1,2...,n}
    val exampleGraph: List[(Int, List[Int])] = List(    // (2,1), (1,3)                 ->  (1, [3, 5])
      (1, List(2, 3)),                                  //(1,3), (5,3)                  ->  (2, [1, 5])
      (3, List(1, 5)),                                  //(5,2)                         ->  (3, [1])
      (2, List(5)),
      (5, List())                                       //                              ->  (5, [2,3])*/
    )

    println(invertGraph(exampleGraph).toString)
  }
  private def invertGraph(exampleGraph: List[(Int, List[Int])]): List[(Int, List[Int])] = {
    exampleGraph.map(vertexDef => { // Mapper
      vertexDef._2.map(neighbour => (neighbour, List(vertexDef._1))).to(LazyList)
    }).foldLeft(mutable.Map[Int, List[Int]]()) {
      (mapCollector, workerData) => { // Reducer
        workerData.foreach(entry => mapCollector(entry._1) = mapCollector.getOrElse(entry._1, List()).appendedAll(entry._2))
        mapCollector
      }
    }.toList
  }
}
