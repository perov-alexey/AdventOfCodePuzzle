object _08 extends App {

  def compare(first: Int, second: Int, condition: String): Boolean = {
    condition match {
      case ">" => first > second
      case ">=" => first >= second
      case "<" => first < second
      case "<=" => first <= second
      case "==" => first == second
      case "!=" => first != second
    }
  }

  def firstStage() = {
    var highestValue = Int.MinValue
    var registers = collection.mutable.Map.empty[String, Int]
    for (line <- Util.read()) {
      val pattern = "(\\w+) (inc|dec) (-?\\d+) if (\\w+) ([<>!=]+) (-?\\d+)".r
      val pattern(changedRegisterName, command, value, conditionRegisterName, condition, conditionValue) = line
      val changedRegisterValue = registers.getOrElse(changedRegisterName, {
        registers += (changedRegisterName -> 0)
        registers(changedRegisterName)
      })
      val conditionRegisterValue = registers.getOrElse(conditionRegisterName, {
        registers += (conditionRegisterName -> 0)
        registers(conditionRegisterName)
      })
      if (compare(conditionRegisterValue, conditionValue.toInt, condition)) {
        command match {
          case "inc" => registers(changedRegisterName) = registers(changedRegisterName) + value.toInt
          case "dec" => registers(changedRegisterName) = registers(changedRegisterName) - value.toInt
        }
      }
    }
    println(registers.maxBy(_._2)._2)
  }

  def secondStage() = {
    var highestValue = Int.MinValue
    var registers = collection.mutable.Map.empty[String, Int]
    for (line <- Util.read()) {
      val pattern = "(\\w+) (inc|dec) (-?\\d+) if (\\w+) ([<>!=]+) (-?\\d+)".r
      val pattern(changedRegisterName, command, value, conditionRegisterName, condition, conditionValue) = line
      val changedRegisterValue = registers.getOrElse(changedRegisterName, {
        registers += (changedRegisterName -> 0)
        registers(changedRegisterName)
      })
      val conditionRegisterValue = registers.getOrElse(conditionRegisterName, {
        registers += (conditionRegisterName -> 0)
        registers(conditionRegisterName)
      })
      if (compare(conditionRegisterValue, conditionValue.toInt, condition)) {
        command match {
          case "inc" => registers(changedRegisterName) = registers(changedRegisterName) + value.toInt
          case "dec" => registers(changedRegisterName) = registers(changedRegisterName) - value.toInt
        }
      }
      if (registers(changedRegisterName) > highestValue) highestValue = registers(changedRegisterName)
    }
    println(highestValue)
  }

  secondStage()

}
