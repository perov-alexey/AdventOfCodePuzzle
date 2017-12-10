import scala.collection.mutable.ArrayBuffer

object _07 extends App {

  class Tree(val name: String, val weight: Int, var parent: Tree) {

    var nodes = new ArrayBuffer[Tree]()

    def find(name : String): Tree = {
      var foundChild: Tree = null
      if (this.name.equals(name)) {
        foundChild = this
      } else {
        for (child <- nodes) {
          val result = child.find(name)
          if (result != null) foundChild = result
        }
      }
      foundChild
    }

    def replace(node: Tree): Unit = {
      val toReplace = this.find(node.name)
      if (toReplace.parent != null) {
        toReplace.parent.nodes = toReplace.parent.nodes.filter(parentNode => !parentNode.name.equals(node.name))
        toReplace.parent.nodes += node
        node.parent = toReplace.parent
      }
    }

    def replaceInChildren(node: Tree): Unit = {
      this.nodes = this.nodes.filter(child => !child.name.equals(node.name))
      this.nodes += node
      node.parent = this
    }

    def findWeight(): Int = {
      var result = this.weight
      for (child <- this.nodes) {
        result += child.findWeight()
      }
      result
    }

    def getBalanceValue(normalValue: Int): Int = {
      var result = 0
      val weights = for (child <- this.nodes) yield (child, child.findWeight())
      val weightsMap = weights.groupBy(_._2)
      if (weightsMap.size > 1) {
        val differentNode = weightsMap.filter(group => group._2.size == 1).head._2(0)._1
        result = differentNode.getBalanceValue(weightsMap.filter(group => group._2.size > 1).head._1)
      } else {
        result = this.weight - (this.findWeight() - normalValue)
      }
      result
    }
  }

  def firstStage() = {
    var freeNodes = new ArrayBuffer[Tree]()
    for (line <- Util.read()) {
      val pattern = "(^\\w+) \\((\\d+)\\)(?: -> ([\\w\\, ]*))?".r
      val pattern(name, weight, nodes) = line
      val newNode = new Tree(name, weight.toInt, null)
      if (nodes != null) {
        for (childNodeName <- nodes.split(", ")) {
          newNode.nodes += new Tree(childNodeName, 0, newNode)
        }

        for (childNodeName <- nodes.split(", ")) {
          for (freeNode <- freeNodes) {
            if (freeNode.name == childNodeName) {
              newNode.replaceInChildren(freeNode)
              freeNodes = freeNodes - freeNode
            }
          }
        }
      }
      var isAdd = true
      for (freeNode <- freeNodes) {
        if (freeNode.find(newNode.name) != null) {
          isAdd = false
          freeNode.replace(newNode)
        }
      }
      if (isAdd) freeNodes += newNode
      println(s"Free Nodes Size: ${freeNodes.size}, Root Node: ${freeNodes(0).name}")
    }
  }

  def secondStage() = {
    var freeNodes = new ArrayBuffer[Tree]()
    for (line <- Util.read()) {
      val pattern = "(^\\w+) \\((\\d+)\\)(?: -> ([\\w\\, ]*))?".r
      val pattern(name, weight, nodes) = line
      val newNode = new Tree(name, weight.toInt, null)
      if (nodes != null) {
        for (childNodeName <- nodes.split(", ")) {
          newNode.nodes += new Tree(childNodeName, 0, newNode)
        }

        for (childNodeName <- nodes.split(", ")) {
          for (freeNode <- freeNodes) {
            if (freeNode.name == childNodeName) {
              newNode.replaceInChildren(freeNode)
              freeNodes = freeNodes - freeNode
            }
          }
        }
      }
      var isAdd = true
      for (freeNode <- freeNodes) {
        if (freeNode.find(newNode.name) != null) {
          isAdd = false
          freeNode.replace(newNode)
        }
      }
      if (isAdd) freeNodes += newNode
    }
    println(freeNodes(0).getBalanceValue(0))
  }

  secondStage()

}
