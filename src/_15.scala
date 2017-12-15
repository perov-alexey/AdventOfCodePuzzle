import scala.io.StdIn

object _15 extends App {

  class NewGenerator(var previous: Long, factor: Int, multiplicity: Int) {
    def next(): Long = {
      do {
        previous = previous * factor % 2147483647
      } while (previous % multiplicity != 0)
      previous
    }
  }

  class Generator(var previous: Long, factor: Int) {
    def next(): Long = {
      previous = previous * factor % 2147483647
      previous
    }
  }

  def firstStage() = {
    val genA = new Generator(StdIn.readInt(), 16807)
    val genB = new Generator(StdIn.readInt(), 48271)
    var scores = 0
    for (i <- 0 until 40000000) {
      val genANext = BigInt(genA.next().toString, 10).toString(2).reverse.padTo(32, "0").reverse.mkString.substring(16)
      val genBNext = BigInt(genB.next().toString, 10).toString(2).reverse.padTo(32, "0").reverse.mkString.substring(16)
      if (genANext.equals(genBNext)) scores += 1
    }
    println(scores)
  }

  def secondStage() = {
    val genA = new NewGenerator(StdIn.readInt(), 16807, 4)
    val genB = new NewGenerator(StdIn.readInt(), 48271, 8)
    var scores = 0
    for (i <- 0 until 5000000) {
      val genANext = BigInt(genA.next().toString, 10).toString(2).reverse.padTo(32, "0").reverse.mkString.substring(16)
      val genBNext = BigInt(genB.next().toString, 10).toString(2).reverse.padTo(32, "0").reverse.mkString.substring(16)
      if (genANext.equals(genBNext)) scores += 1
    }
    println(scores)
  }

  secondStage()

}
