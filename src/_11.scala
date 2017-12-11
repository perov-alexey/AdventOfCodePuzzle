import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object _11 extends App {

  def calculateSteps(x: Int, y: Int): Int = {
    val normilizedX = Math.abs(x)
    val normilizedY = Math.abs(y)
    var diagonalSteps = (normilizedX / 3).min(normilizedY / 3)
    diagonalSteps + (normilizedX - 3 * diagonalSteps + normilizedY - 3 * diagonalSteps) / 6
  }

  def firstStage() = {
    val path = StdIn.readLine().split(",")
    var x = 0
    var y = 0
    for (direction <- path) {
      direction match {
        case "n" => y += 6
        case "s" => y -= 6
        case "e" => x += 6
        case "w" => x -= 6
        case "ne" => x += 3; y += 3
        case "se" => x += 3; y -= 3
        case "sw" => x -= 3; y -= 3
        case "nw" => x -= 3; y += 3
      }
    }
    println(calculateSteps(x, y))
  }

  def secondStage() = {
    val path = StdIn.readLine().split(",")
    var x = 0
    var y = 0
    var furthestStep = 0
    for (direction <- path) {
      direction match {
        case "n" => y += 6
        case "s" => y -= 6
        case "e" => x += 6
        case "w" => x -= 6
        case "ne" => x += 3; y += 3
        case "se" => x += 3; y -= 3
        case "sw" => x -= 3; y -= 3
        case "nw" => x -= 3; y += 3
      }
      if (calculateSteps(x, y) > furthestStep) {
        furthestStep = calculateSteps(x, y)
      }
    }
    println(furthestStep)
  }

  secondStage()

}
