object _13 extends App {

  class Scanner(var name: String, var range: Int) {

    var direction = 1
    var position = 0

    def move(): Unit = {
      position += direction
      if (position == 0 || position == range - 1) {
        direction *= -1
      }
    }

  }

  def firstStage() = {
    val definitions = Util.read()
    val scanners = new Array[Scanner](definitions.last.split(":")(0).toInt + 1)
    definitions.foreach(definition => {
      val regex = """(\d+): (\d+)""".r
      val regex(depth, range) = definition
      scanners(depth.toInt) = new Scanner(depth, range.toInt)
    })

    var scores = 0
    for (step <- 0 until scanners.length) {
      if (scanners(step) != null && scanners(step).position == 0) {
        scores += step * scanners(step).range
      }
      scanners.filter(_ != null).foreach(_.move())
    }

    println(scores)

  }

  def secondStage() = {
    val definitions = Util.read()
    val scanners = definitions.map(definition => {
      val regex = """(\d+): (\d+)""".r
      val regex(depth, range) = definition
      (depth, range)
    }).toMap

    var isCatched = true
    var delay = 0
    while (isCatched) {
      isCatched = false

      val keysIterator = scanners.keysIterator
      while (keysIterator.hasNext) {
        val depth = keysIterator.next()
        if ((depth.toInt + delay) % (scanners(depth).toInt * 2 - 2) == 0) isCatched = true
      }

      delay += 1
    }
    println(delay)

  }

  secondStage()

}
