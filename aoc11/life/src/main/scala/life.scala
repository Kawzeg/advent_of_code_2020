import io.Source.fromFile
import scala.annotation.tailrec

object Cell extends Enumeration {
  type State = Value
  val Alive = Value("#")
  val Dead = Value("L")
  val Empty = Value(".")

  def parse(c: Char): State = c match {
    case 'L' => Dead
    case '#' => Alive
    case '.' => Empty
  }
}

object Game {
  type Board = Array[Array[Cell.State]]
  type Rules = (Board, Cell.State, Int, Int) => Cell.State

  def draw(board: Board) =
    for (line <- board) {
      line.foreach(print)
      println()
    }
  def next(board: Board, rules: Rules): (Board, Boolean) = {
    var changed = false
    val b = board.zipWithIndex.map{case (row, y) => {
      row.zipWithIndex.map{case (cell, x) => {
        val newCell = rules(board, cell, x, y)
        if (newCell != cell) changed = true
        newCell
      }}
    }}
    (b, changed)
  }
  def run(board: Board, rules: Rules): Board = {
    next(board, rules) match {
      case (b, false) => b
      case (b, true) => run(b, rules)
    }
  }
}

object Main extends App {
  // Helper for visibleFrom
  @tailrec
  def look(board: Board, x: Int, y: Int, dx: Int, dy: Int, further: Boolean): Cell.State = {
    val nx = x+dx
    val ny = y+dy
    if (nx < 0 || nx >= w) return Cell.Empty
    if (ny < 0 || ny >= h) return Cell.Empty
    board(ny)(nx) match {
      case Cell.Empty if further => look(board, nx, ny, dx, dy, further)
      case c => c
    }
  }
  type View = (Board, Int, Int) => Seq[Cell.State] // Where do people look
  // further = true for part 2
  def visibleFrom(further: Boolean): View = (board, x, y) =>
    for {
      dx <- -1 to 1
      dy <- -1 to 1
      if (dx != 0 || dy != 0) } yield look(board, x, y, dx, dy, further)

  def makeRules(rule: Int, view: View): Game.Rules = (board, cell, x, y) =>
    cell match {
      case Cell.Empty => Cell.Empty
      case state => view(board, x, y).filter(_ == Cell.Alive).length match {
        case 0 if state == Cell.Dead => Cell.Alive
        case n if state == Cell.Alive && n >= rule => Cell.Dead
        case _ => state
      }
    }

  type Board = Game.Board
  val input = fromFile("src/main/resources/input").getLines.map(_.toArray).toArray
  val w = input(0).length
  val h = input.length
  printf("Width:  %s\nHeight: %s\n", w, h)

  val board = input.map(_.map(Cell.parse))

  def count(b: Board) = b.flatMap(_.map(x => if (x == Cell.Alive) 1 else 0)).sum
  var part1 = Game.run(board, makeRules(4, visibleFrom(false)))
  printf("Part 1 Alive seats: %s\n", count(part1))
  var part2 = Game.run(board, makeRules(5, visibleFrom(true)))
  printf("Part 2 Alive seats: %s\n", count(part2))
}
