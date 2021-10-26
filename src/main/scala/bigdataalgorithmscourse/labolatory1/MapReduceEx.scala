package bigdataalgorithmscourse.labolatory1

import scala.collection.mutable

object MapReduceEx {

  def main(args: Array[String]): Unit = {

    // Let's assume that V = {1,2...,n}
    val exampleGraph: List[(Int, List[Int])] = List(    // (2,1), (1,3)                 ->  (1, [3, 5])
      (1, List(2, 3)),                                  //(1,3), (5,3)                  ->  (2, [1, 5])
      (3, List(1, 5)),                                  //(5,2)                         ->  (3, [5])
      (2, List(5)),                                     //(1,5),(2,5),(3,5),(4,5),(5,5) ->  (4, [5])
      (5, List())                                       //                              ->  (5, [2,3,5])*/
    )

    println(invertGraph(exampleGraph).toString)
  }

  private def invertGraph(exampleGraph: List[(Int, List[Int])]): List[(Int, List[Int])] = {
    val maxVertexId = exampleGraph.map(x => x._1).max
    exampleGraph.map(vertexDef => { // Mapper
      val neighbours = vertexDef._2
      if (neighbours.isEmpty) {
        Range(1, maxVertexId + 1).map(neighbour => (neighbour, List(vertexDef._1))).to(LazyList)
      } else {
        neighbours.map(neighbour => (neighbour, List(vertexDef._1))).to(LazyList)
      }
    }).foldLeft(mutable.Map[Int, List[Int]]()) {
      (mapCollector, workerData) => { // Reducer
        workerData.foreach(entry => mapCollector(entry._1) = mapCollector.getOrElse(entry._1, List()).appendedAll(entry._2))
        mapCollector
      }
    }.toList
  }
}
