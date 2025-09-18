package logic

sealed abstract class Direction 

case class East()   extends Direction  {  }
case class North()  extends Direction  {  }
case class West()   extends Direction  { }
case class South()  extends Direction  {}