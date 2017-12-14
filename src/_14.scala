import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object _14 extends App {

  def knotHash(key: String): String = {
    var data = (for (value <- 0 to 255) yield value).toArray
    val inputs = key.map(_.toInt) ++ Array(17, 31, 73, 47, 23)
    var denseHash = new ArrayBuffer[String]()
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
      denseHash += result.toHexString.reverse.padTo(2, "0").reverse.mkString
    }
    denseHash.mkString("")
  }


  def findGroup(memory: Array[Array[String]], lastSectorNumber: Int, i: Int, j: Int): Unit = {
    memory(i)(j) = lastSectorNumber.toString
    val neighbours = Array((i, j + 1), (i - 1, j), (i + 1, j), (i, j - 1))
    for (coordinates <- neighbours) {
      try {
        if ("#".equals(memory(coordinates._1)(coordinates._2))) {
          findGroup(memory, lastSectorNumber, coordinates._1, coordinates._2)
        }
      } catch {
        case _: Throwable => //It's ok
      }
    }
  }

  def secondStage() = {
    var lastSectorNumber = 0
    val memory = Array.ofDim[String](128, 128)
    val key = StdIn.readLine()
    for (i <- 0 until memory.length) {
      memory(i) = BigInt(knotHash(s"$key-$i"), 16).toString(2).reverse.padTo(128, "0").reverse.mkString.
        replace("1", "#").split("")
    }
    for (i <- 0 until memory.length) {
      for (j <- 0 until memory(i).length) {
        if ("#".equals(memory(i)(j))) {
          lastSectorNumber += 1
          findGroup(memory, lastSectorNumber, i, j)
        }
      }
    }
    println(lastSectorNumber)
  }

  secondStage()

}
