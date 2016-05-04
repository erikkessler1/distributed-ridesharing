object ANSI {

  val BOLD = 1
  val UNDERLINE = 4

  val clear = "\u001b[2J"

  def move(row: Int, col: Int) = s"\u001b[${row};${col}H"

  def down(lines: Int) = s"\u001b[${lines}B"

  def style(style: List[Int], text: String) = s"\u001b[${style.mkString(";")}m${text}\u001b[0m"

}
