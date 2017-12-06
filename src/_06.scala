import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object _06 extends App {

  def firstStage() = {
    val history = new ArrayBuffer[Array[Int]]()
    val memory = StdIn.readLine().split("\t").map(element => element.toInt)
    var latestState = memory
    var amountOfSteps = 0

    while (history.count(element => latestState.deep == element.deep) == 0) {
      history += latestState.clone
      val maxElementIndex = latestState.zipWithIndex.maxBy(_._1)._2
      val maxElement = latestState(maxElementIndex)
      latestState(maxElementIndex) = 0
      for (i <- (maxElementIndex + 1) to (maxElementIndex + maxElement) ) {
        latestState(i % latestState.length) += 1
      }
      amountOfSteps += 1
    }

    println(amountOfSteps)

  }

  def secondStage() = {
    val history = new ArrayBuffer[Array[Int]]()
    val memory = StdIn.readLine().split("\t").map(element => element.toInt)
    var latestState = memory
    var amountOfSteps = 0

    while (history.count(element => latestState.deep == element.deep) == 0) {
      history += latestState.clone
      val maxElementIndex = latestState.zipWithIndex.maxBy(_._1)._2
      val maxElement = latestState(maxElementIndex)
      latestState(maxElementIndex) = 0
      for (i <- (maxElementIndex + 1) to (maxElementIndex + maxElement) ) {
        latestState(i % latestState.length) += 1
      }
      amountOfSteps += 1
    }

    println(amountOfSteps)
    println(history.length - history.zipWithIndex.filter(tuple => tuple._1.deep == latestState.deep)(0)._2)
  }

  secondStage()

}
