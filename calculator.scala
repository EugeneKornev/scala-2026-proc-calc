import scala.util.boundary, boundary.break

/** Software implementation of PROC (PROstoy Calculator) mk. 1 (or mk. 2).
  *
  * You should finish this procedure according to
  * the reference described in `README.md` to complete
  * the assignment.
  */
@main def calculator(commands: String*): Unit = {
  /** Converts given string `s` to integer.
    *
    * Throws [[NumberFormatException]] if `s` can't be converted to integer,
    * but you shouldn't worry about it at this moment.
    */
  def parseInt(s: String): Int = s.toInt

  /** Representation of `acc` register. */
  var acc: Int = 0

  var A: Int = 0
  var B: Int = 0

  var blink: Boolean = false

  boundary:
    for c <- commands do 
      c match {
        case "+" =>
          acc = A + B
          blink = false
        case "-" =>
          acc = A - B
          blink = false
        case "*" =>
          acc = A * B
          blink = false
        case "/" =>
          acc = if B != 0 then A / B else 0
          blink = false
        case "swap" =>
          val (newA, newB) = (B, A)
          A = newA
          B = newB
        case "blink" =>
          blink = !blink
        case "acc" =>
          if blink then B = acc else A = acc
          blink = !blink
        case "break" =>
          break()
        case other =>
          val tmp = parseInt(c)
          if blink then B = tmp else A = tmp
          blink = !blink
      }
  println(acc)
}
