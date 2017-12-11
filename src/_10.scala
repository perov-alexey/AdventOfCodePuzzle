import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object _10 extends App {

  def firstStage() = {
    var data = (for (value <- 0 to 255) yield value).toArray
    val inputs = StdIn.readLine().split(",").map(_.toInt)
    var pointer = 0
    var skipSize = 0
    for (input <- inputs) {
      var subarray = data
      if (pointer + input <= data.length) {
        subarray = subarray.patch(pointer, subarray.slice(pointer, pointer + input).reverse, input)
      } else {
        val toReplace = subarray.slice(pointer, pointer + input) ++ subarray.slice(0, (pointer + input) % subarray.size)
        subarray = subarray.patch(pointer, toReplace.reverse.slice(0, subarray.size - pointer), input)
        subarray = subarray.patch(0, toReplace.reverse.slice(subarray.size - pointer, toReplace.length), (pointer + input) % subarray.size)
      }
      data = subarray
      pointer = (pointer + input + skipSize) % data.length
      skipSize += 1
    }
    println(data(0) * data(1))
  }

  def secondStage() = {
    var data = (for (value <- 0 to 255) yield value).toArray
    var denseHash = new ArrayBuffer[String]()
    val inputs = StdIn.readLine().toList.map(_.toInt) ++ Array(17, 31, 73, 47, 23)
    var pointer = 0
    var skipSize = 0
    for (round <- 0 until 64) {
      for (input <- inputs) {
        var subarray = data
        if (pointer + input <= data.length) {
          subarray = subarray.patch(pointer, subarray.slice(pointer, pointer + input).reverse, input)
        } else {
          val toReplace = subarray.slice(pointer, pointer + input) ++ subarray.slice(0, (pointer + input) % subarray.size)
          subarray = subarray.patch(pointer, toReplace.reverse.slice(0, subarray.size - pointer), input)
          subarray = subarray.patch(0, toReplace.reverse.slice(subarray.size - pointer, toReplace.length), (pointer + input) % subarray.size)
        }
        data = subarray
        pointer = (pointer + input + skipSize) % data.length
        skipSize += 1
      }
    }
    for (i <- 0 until 16) {
      var result = 0
      for (j <- 0 until 16) {
        result = result ^ data(i * 16 + j)
      }
      denseHash += result.toHexString
    }
    println(denseHash.mkString)
  }

  secondStage()

}
