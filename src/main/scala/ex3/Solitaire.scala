package ex3

object Solitaire extends App:
  val rows = 6
  val cols = 6
  // With a 6x6 grid i obtain 101642 valid results
  val MaxNumber = rows * cols
  val startRow = 2
  val startCol = 2

  val moves = List(
    (3, 0), (-3, 0), (0, 3), (0, -3), // Horizontal and vertical moves
    (2, 2), (2, -2), (-2, 2), (-2, -2) // Diagonal moves
  )

  type Position = (Int, Int)
  var solutions: List[List[Position]] = List()

  def render(solution: Seq[Position], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
            number = reversed.indexOf((x,y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def isSafe(board: Array[Array[Int]], row: Int, col: Int): Boolean =
    if (row >= 0 && row < rows && col >= 0 && col < cols && board(row)(col) == 0)
      true
    else
      false

  def solve(board: Array[Array[Int]],row: Int, col: Int,num: Int, sol: List[Position]): Boolean =
    if (num > MaxNumber) {
      solutions = sol :: solutions
      return true
    }
    var found = false
    for ((permittedRow, permittedCol) <- moves) {
      val newRow = row + permittedRow
      val newCol = col + permittedCol
      if (isSafe(board, newRow, newCol)) {
        board(newRow)(newCol) = num
        if (solve(board, newRow, newCol, num + 1, (newRow, newCol) :: sol)) found = true
        board(newRow)(newCol) = 0 // Backtrack
      }
    }
    found

  val board: Array[Array[Int]] = Array.ofDim[Int](rows, cols)
  board(startRow)(startCol) = 1

  solve(board, startRow, startCol, 2, List((startRow, startCol)))
  println(solutions.length)
  //println(solutions.head)
  //solutions.foreach(println(_))
  println(render(solutions.head, cols ,rows ))
  //println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))