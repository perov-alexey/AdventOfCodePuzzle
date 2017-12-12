import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object _12 extends App {

  class Graph() {
    private val vertexMap = new mutable.HashMap[String, Set[String]]()

    def addEdge(vertexName1: String, vertexName2: String) = {
      if (!vertexMap.contains(vertexName1)) vertexMap.put(vertexName1, Set[String]())
      if (!vertexMap.contains(vertexName2)) vertexMap.put(vertexName2, Set[String]())
      vertexMap(vertexName1) += vertexName2
      vertexMap(vertexName2) += vertexName1
    }

    def findSubgraph(root: String, consideredVertexes: ArrayBuffer[String]): Array[String] = {
      var result = Array(root).toBuffer
      consideredVertexes += root
      for (vertex <- vertexMap(root)) {
        if (!consideredVertexes.contains(vertex)) {
          result = result ++ findSubgraph(vertex, consideredVertexes)
        }
      }
      result.toArray
    }

    def getSize(): Int = {
      vertexMap.size
    }

    def remove(vertex: String): Unit = {
      vertexMap.remove(vertex)
    }

    def firstVertexIndex(): String = {
      vertexMap.keySet.head
    }
  }

  def firstStage() = {
    val graph = new Graph()
    for (line <- Util.read()) {
      val pattern = """(\d+) <-> (.+)""".r
      val pattern(name, relations) = line
      for (relation <- relations.split(", ")) graph.addEdge(name, relation)
    }
    println(graph.findSubgraph("0", new ArrayBuffer[String]()).length)
  }

  def secondStage() = {
    val graph = new Graph()
    for (line <- Util.read()) {
      val pattern = """(\d+) <-> (.+)""".r
      val pattern(name, relations) = line
      for (relation <- relations.split(", ")) graph.addEdge(name, relation)
    }

    var groupsAmount = 0
    while (graph.getSize > 0) {
      groupsAmount += 1
      for (vertex <- graph.findSubgraph(graph.firstVertexIndex, new ArrayBuffer[String]())) graph.remove(vertex)
    }
    println(groupsAmount)
  }

  secondStage()

}
