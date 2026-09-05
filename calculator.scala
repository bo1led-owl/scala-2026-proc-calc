import scala.util.boundary
import scala.util.boundary.break

@main def calculator(commands: String*): Unit = {
  var calc = Calc.zeroed

  boundary:
    for cmd <- commands do {
      calc = cmd.trim match
        case "+"     => calc.add
        case "-"     => calc.sub
        case "*"     => calc.mul
        case "/"     => calc.div
        case "swap"  => calc.swap
        case "blink" => calc.blink
        case "acc"   => calc.load(calc.acc)
        case "break" => break()
        case s       => calc.load(s.toInt)
    }

  println(calc.acc)
}

object Calc {
  def zeroed: Calc = Calc(0, 0, 0, false)
}

case class Calc(acc: Int, a: Int, b: Int, flag: Boolean) {
  def add: Calc = copy(acc = a + b).clearFlag
  def sub: Calc = copy(acc = a - b).clearFlag
  def mul: Calc = copy(acc = a * b).clearFlag
  def div: Calc = if b != 0 then copy(acc = a / b).clearFlag else Calc.zeroed

  def blink: Calc     = copy(flag = !flag)
  def clearFlag: Calc = copy(flag = false)

  def swap: Calc = copy(a = b, b = a)
  def load(x: Int): Calc = (if !flag then copy(a = x) else copy(b = x)).blink
}
