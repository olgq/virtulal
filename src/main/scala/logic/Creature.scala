package logic

import java.lang.annotation.Target

class Creature(x:Int, y:Int) {
  var state: State = State(100, 100, Point(x, y), North(), None, Point(x,y))

  val EnergyPerMove: Int = 1

  def setTarget(p: Option[Point]): Unit = {
    state = state.copy(target = p)
  }
  def clearTarget(): Unit = {
    state = state.copy(target = None)
  }

  def consumeEnergy(amount: Int): Unit = {
    state = state.copy(energy = state.energy - amount)
    if (!enoughEnergy()) {
      setTargetIfNone(findNearest(ElixirWell()))
    }
  }

  def enoughEnergy(): Boolean = {
    state.energy > 15
  }

  def updateTarget(target: Option[Point]): Unit = {
    if (state.position == target) {
      clearTarget()
    } else {
      setTarget(target)
    }
  }
  def setTargetIfNone(p: Option[Point]): Unit = {
    if (state.target.isEmpty) {
      setTarget(p)
    }
  }
  def findNearest(obj: CellType): Option[Point] = {
    if (state.memory.isEmpty) return None
    val filtered = state.memory.filter(_._2 == obj)
    if (filtered.isEmpty) return None
    Some(filtered.minBy{ case (point, _) =>
      math.abs(point.x - state.position.x) + math.abs(point.y - state.position.y)
    }._1)
  }

  def explore() : Direction = {
    val directions = List(North(), East(), South(), West())

    directions(scala.util.Random.nextInt(directions.length))
  }

  def goToTarget(target: Point) :Direction = { // TODO improve to go diagonally
    val dx = target.x - state.position.x
    val dy = target.y - state.position.y

    if (math.abs(dx) > math.abs(dy)) {
      if (dx > 0) East() else West()
    } else {
      if (dy > 0) South() else North()
    }
  }

  def movePosition(d: Direction, isEmpty: Point => Boolean): Unit = {
    d match {
      case North() => if (isEmpty(nextPosition(d)))
        state = state.copy(lastPosition = state.position,position = nextPosition(d))
      case South() => if (isEmpty(nextPosition(d)))
        state = state.copy(lastPosition = state.position,position = nextPosition(d))
      case East() => if (isEmpty(nextPosition(d)))
        state = state.copy(lastPosition = state.position,position = nextPosition(d))
      case West() => if (isEmpty(nextPosition(d)))
        state = state.copy(lastPosition = state.position,position = nextPosition(d))
    }
  }

  private def nextPosition(d: Direction): Point = {
    d match {
      case North() => Point(state.position.x, state.position.y - 1)
      case South() => Point(state.position.x, state.position.y + 1)
      case East() => Point(state.position.x + 1, state.position.y)
      case West() => Point(state.position.x - 1, state.position.y)
    }
  }

  def move(isEmpty: Point => Boolean, getCellType: Point => CellType): Unit = {

    val d: Direction = state.target match {
      case Some(t) => goToTarget(t)
      case None => explore()
    }
    movePosition(d, isEmpty)
    state = state.copy(direction = d)
    consumeEnergy(EnergyPerMove)
    perceive(getCellType)
    println("energy: " + state.energy + " position: " + state.position + " target: " + state.target + " direction: " + state.direction)
    println("memory: " + state.memory)
  }

  def perceive(getCellType: Point => CellType): Unit = {
    val directions = List(North(), East(), South(), West())
    val perceivedPoints = directions.map { d =>
      (d, nextPosition(d))
    } :+ (null, state.position) // also perceive current position

    val newMemoryEntries = perceivedPoints.flatMap { case (_, p) =>
      val cellType = getCellType(p)
      if (cellType != Empty() && cellType != Wall() && cellType != Body()) Some(p -> cellType) else None
    }.toMap

    state = state.copy(memory = state.memory ++ newMemoryEntries)
  }




}

