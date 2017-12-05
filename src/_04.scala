import scala.io.StdIn

object _04 extends App {

  def firstStage() = {
    var amountOfPassphrases = 0

    while (true) {
      var words = StdIn.readLine().split(" ")
      var wordsMap = collection.mutable.Set[String]()
      for (word <- words) wordsMap = wordsMap + word
      if (wordsMap.size == words.length) amountOfPassphrases += 1
      println(amountOfPassphrases)
    }
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

  secondStage()

}
