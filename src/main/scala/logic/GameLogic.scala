package logic

class GameLogic(x: Int, y: Int) {
  val creatures: List[Creature] = List(new Creature(2,5))
  val grid: Array[Array[Int]] = Array.ofDim[Int](x, y)


  def getCellType(p: Point): CellType = {
    for (c <- creatures) {
      if (c.state.position == p) return Body()
    }
    if (p.x < 0 || p.y < 0 || p.x >= x || p.y >= y) Wall()
    else if (p == Food()) Food()
    else if (p == Wall()) Wall()
    else if (p == Body()) Body()
    else Empty()
  }

  def step(): Unit ={
    for (c <- creatures) {
      c.move(isEmpty)
    }
  }

  def isEmpty(p: Point): Boolean =
    !(p.x < 0 || p.y < 0 || p.x >= x || p.y >= y - 1) && !creatures.exists(_.state.position == p)
}
