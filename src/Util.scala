import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Util {

  def read(): Array[String] = {
    var isReaded = false
    val result = new ArrayBuffer[String]()
    while(!isReaded) {
      var input = StdIn.readLine()
      if (input.equals("exit")) {
        isReaded = true
      } else {
        result += input
      }
    }
    result.toArray
  }

}
