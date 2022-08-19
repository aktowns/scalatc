package planners.astar

case class Node(
    pos: Position,
    restricted: Boolean,
    gScore: Int, // = 0, // The total cost of getting to that node
    heuristic: Int, // = 0, // The estimated time to reach the finish from the current node.
    visited: Boolean, // = false,
    closed: Boolean, // = false,
    parent: Option[Node] // = None
) extends Ordered[Node]:
  lazy val fScore: Int = gScore + heuristic
  lazy val row: Int = pos.x
  lazy val col: Int = pos.y

  def update(gg: Int, hh: => Int): Node =
    val updated = this.copy(gScore = gg, visited = true)
    if heuristic == 0 then updated.copy(heuristic = hh) else updated

  def compare(that: Node): Int = that.fScore.compare(fScore)
