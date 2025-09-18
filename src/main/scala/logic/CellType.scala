package logic


import logic.CellType

sealed abstract class CellType

case class Empty() extends CellType
case class Wall() extends CellType
case class Food() extends CellType
case class Body() extends CellType