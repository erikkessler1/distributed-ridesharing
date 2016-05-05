object ANSI {

  // Style options
  val BOLD = 1
  val BRIGHT = 1
  val UNDERLINE = 4
  val PURPLE = 35
  val CYAN = 36
  val GRAY = 37

  val clear = "\u001b[2J"

  val delete = "\u001b[1K"

  val store = "\u001b[s"

  val restore = "\u001b[u"

  // Cursor movement
  def move(row: Int, col: Int) = s"\u001b[${row};${col}H"

  def up(rows: Int) = s"\u001b[${rows}A"

  def down(rows: Int) = s"\u001b[${rows}B"

  def right(cols: Int) = s"\u001b[${cols}C"

  def left(cols: Int) = s"\u001b[${cols}D"

  def style(style: List[Int], text: String) = s"\u001b[${style.mkString(";")}m${text}\u001b[0m"

}
