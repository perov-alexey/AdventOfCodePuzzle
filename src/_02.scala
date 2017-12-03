import scala.io.StdIn

object _02 extends App {

  def firstStage() = {
    var sum = 0

    while (true) {
      var line = StdIn.readLine().split("\t").map(element => element.toInt)
      sum = sum + line.max - line.min
      println(sum)
    }
  }

  def secondStage() = {
    var sum = 0f

    while (true) {
      var line = StdIn.readLine().split("\t").map(element => element.toFloat)
      for (i <- 0 until line.length) {
        for (j <- 0 until line.length) {
          if (line(i) != line(j) && line(i) % line(j) == 0) {
            sum = sum + line(i) / line(j)
          }
        }
      }

      println(sum)
    }
  }

  secondStage()

}
