package planners

import atc.v1 as atc

case class Position(x: Int, y: Int)

object Position:
  def fromModel(model: atc.Node): Position =
    Position(-model.latitude, -model.longitude)
