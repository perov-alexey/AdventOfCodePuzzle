import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object _09 extends App {

  def firstStage() = {
    var stream = StdIn.readLine()
    stream = stream.replaceAll("""!.{1}""", "")
    stream = stream.replaceAll("""<.*?>""", "")
    stream = stream.replaceAll(""",""", "")

    var stack = new ArrayBuffer[Char]()
    var scores = 0
    for (bracket <- stream) {
      bracket match {
        case '{' => stack += bracket
        case '}' => {
          scores += stack.size
          stack.remove(0)
        }
      }
    }
    println(scores)
  }

  def secondStage() = {
    var scores = 0
    var stream = StdIn.readLine()
    stream = stream.replaceAll("""!.{1}""", "")
    for(group <- """<.*?>""".r.findAllIn(stream).toList) {
      scores += group.length - 2
    }
    println(scores)
  }

  secondStage()

}
