import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object _03 extends App {

  def firstStage() = {
    val number = StdIn.readInt()
    var amountOfNumbersInSide = math.ceil(math.sqrt(number)).toInt
    if (amountOfNumbersInSide % 2 == 0) amountOfNumbersInSide += 1
    val level = amountOfNumbersInSide / 2
    val maxAxisNumber = amountOfNumbersInSide * amountOfNumbersInSide - amountOfNumbersInSide / 2
    val shift = math.min(level * 2 - ((maxAxisNumber - number) % (level * 2)),
      ((maxAxisNumber - number) % (level * 2)))
    println(level + shift)
  }

  def secondStage() = {
    val value = StdIn.readInt()
    val levels = 4
    val array = Array.fill[Array[Int]](levels * 2 + 1)(Array.fill[Int](levels * 2 + 1)(0))

    def buildPath(levels : Int) : Array[(Int, Int)] = {
      val size = levels * 2
      val path = new ArrayBuffer[(Int, Int)]()
      for (level <- 0 until levels) {
        for (i <- size - level to level by -1) path += ((i, size - level))
        for (j <- size - level - 1 to level by -1) path += ((level, j))
        for (k <- level + 1 to size - level) path += ((k, level))
        for (l <- level + 1 to size - level - 1) path += ((size - level, l))
      }
      path += ((levels, levels))
      path.toArray.reverse
    }

    def getValueByNeighbours(array : Array[Array[Int]], coordinates : (Int, Int)): Int = {
      var sum = array(coordinates._1)(coordinates._2)
      try {sum += array(coordinates._1 + 1)(coordinates._2)} catch { case ex : Exception => /** it's ok **/}
      try {sum += array(coordinates._1 + 1)(coordinates._2 - 1) } catch { case ex : Exception => /** it's ok **/}
      try {sum += array(coordinates._1)(coordinates._2 - 1) } catch { case ex : Exception => /** it's ok **/}
      try {sum += array(coordinates._1 - 1)(coordinates._2 - 1) } catch { case ex : Exception => /** it's ok **/}
      try {sum += array(coordinates._1 - 1)(coordinates._2) } catch { case ex : Exception => /** it's ok **/}
      try {sum += array(coordinates._1 - 1)(coordinates._2 + 1) } catch { case ex : Exception => /** it's ok **/}
      try {sum += array(coordinates._1)(coordinates._2 + 1) } catch { case ex : Exception => /** it's ok **/}
      try {sum += array(coordinates._1 + 1)(coordinates._2 + 1) } catch { case ex : Exception => /** it's ok **/}
      sum
    }

    val path = buildPath(levels)
    array(levels)(levels) = 1
    for (coordinates <- path) {
      val newValue = getValueByNeighbours(array, coordinates)
      array(coordinates._1)(coordinates._2) = newValue
      if (newValue >= value) {
        println(newValue)
      }
    }
  }

  secondStage()

}
