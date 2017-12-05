import scala.io.StdIn

object _05 extends App {

  def firstStage() = {
    val maze = Util.read().map(element => element.toInt)
    var isExited = false
    var pointer = 0
    var stepsAmount = 0

    while(!isExited) {
      val oldPointer = pointer
      pointer += maze(pointer)
      if (maze(oldPointer) >= 3) {
        maze(oldPointer) -= 1
      } else {
        maze(oldPointer) += 1
      }


      if (pointer >= maze.length) {
        isExited = true
      }
      stepsAmount += 1
    }

    println(stepsAmount)

  }

  def secondStage() = {
    var amountOfPassphrases = 0

    while (true) {
      var words = StdIn.readLine().split(" ").map(word => word.split("").sorted.mkString)
      var wordsMap = collection.mutable.Set[String]()
      for (word <- words) wordsMap = wordsMap + word
      if (wordsMap.size == words.length) amountOfPassphrases += 1
      println(amountOfPassphrases)
    }
  }

  firstStage()

}
