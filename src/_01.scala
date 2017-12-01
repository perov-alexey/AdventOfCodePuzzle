object _01 extends App{

  def firstStage() = {
    val captcha = scala.io.StdIn.readLine()
    var sum = 0
    for (i <- 0 until captcha.size) {
      if (captcha(i) == captcha((i + 1) % captcha.size)) sum = sum + captcha(i).asDigit
    }
    print(sum)
  }

  def secondStage() = {
    val captcha = scala.io.StdIn.readLine()
    var sum = 0
    for (i <- 0 until captcha.size) {
      if (captcha(i) == captcha((i + captcha.size / 2) % captcha.size)) sum = sum + captcha(i).asDigit
    }
    print(sum)
  }

  secondStage()

}
