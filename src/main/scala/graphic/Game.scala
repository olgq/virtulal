package graphic
import logic._


class Game(x: Int, y: Int) {
  val game = new logic.GameLogic(x, y)

  def drawCell(t: CellType): Unit = {
    t match {
      case Empty() => print(". ")
      case Wall() => print("# ")
      case Food() => print("F ")
      case Body() => print("B ")
      case ElixirWell() => print("E ")
    }
  }

  def drawGrid(): Unit = {
    for (j <- game.grid.indices) {
      for (i <- game.grid(j).indices) {
        drawCell(game.getCellType(Point(i, j)))
      }
      println()
    }
  }


  def updateState(): Unit = {
    Thread.sleep(500)
    val dir = Array(South(), East(), North(), West())
    game.step()
  }

  def loop(): Unit = {
    while (true) {
      drawGrid()
      updateState()
      Thread.sleep(16) // Approx 60 FPS
      println("\u001b[H\u001b[2J")
      System.out.flush()
    }
  }

}
