import scala.util.boundary
import scala.util.boundary.break

@main def calculator(commands: String*): Unit = {
  var calc = Calc(0, 0, 0, false)

  boundary:
    for cmd <- commands do {
      calc = cmd.trim match
        case "+"     => calc.add
        case "-"     => calc.sub
        case "*"     => calc.mul
        case "/"     => calc.div
        case "swap"  => calc.swap
        case "blink" => calc.blink
        case "acc"   => calc.acc
        case "break" => break()
        case s       => calc.load(s.toInt)
    }

  println(calc.accumulator)
}

case class Calc(accumulator: Int, a: Int, b: Int, flag: Boolean) {
  def add: Calc   = copy(accumulator = a + b, flag = false)
  def sub: Calc   = copy(accumulator = a - b, flag = false)
  def mul: Calc   = copy(accumulator = a * b, flag = false)
  def div: Calc   = if b != 0 then copy(accumulator = a / b, flag = false) else Calc(0, 0, 0, false)
  def swap: Calc  = copy(a = b, b = a)
  def blink: Calc = copy(flag = !flag)
  def acc: Calc   = load(accumulator)
  def load(x: Int): Calc =
    val loaded = if !flag then copy(a = x) else copy(b = x)
    loaded.blink
}
