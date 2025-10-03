package logic

case class State(
                  health:        Int,
                  energy:        Int,
                  position:      Point,
                  direction: Direction,
                  target:        Option[Point] = None,
                  lastPosition: Point,
                  memory: Map[Point, CellType] = Map()
                )
