import cats.effect.IO
import atc.v1.{Airplane, Airport, Node, Point}
import ui.v1.NodePoint
import cats.Order
import cats.implicits.*

// |---------(+)----------
// |    2     |     1
// |          |
// (-)-----(0, 0)-------(+)
// |          |
// |    3     |     4
// |---------(-)----------
object SimpleFlightPlanner:
  def quadrant(a: Point): Int =
    if a.x > 0 && a.y > 0 then 1
    else if a.x > 0 && a.y < 0 then 4
    else if a.x < 0 && a.y > 0 then 2
    else if a.x < 0 && a.y < 0 then 3
    else 0

  def distance(a: Point, b: Point): Double =
    math.sqrt(math.pow(b.x - a.x, 2) + math.pow(b.y - a.y, 2) * 1.0)

  def apply(
      airplane: Airplane,
      airports: Seq[Airport],
      grid: Seq[NodePoint]
  ): IO[Seq[Node]] = IO {
    println(
      s"Flight ${airplane.id} is looking to go to ${airplane.tag} airport"
    )

    val lookupPoint = grid.map(np => (np.point.get, np.node.get)).toMap
    val lookupNode: Map[(Int, Int), Point] =
      grid
        .map(np =>
          ((np.node.get.latitude, np.node.get.longitude), np.point.get)
        )
        .toMap
    val targetAirport = airports.find(_.tag == airplane.tag).get

    // val cX = lookupPoint.keys.minBy(v => math.abs(v.x - airplane.point.get.x))
    // val cY = lookupPoint.keys.minBy(v => math.abs(v.y - airplane.point.get.y))

    val start = airplane.point.get

    val TQ = if airplane.tag.isTagBlue then 1 else 3
    val end =
      if quadrant(start) == TQ then
        println(s"Flight ${airplane.id} is in ${TQ} going to airport")
        lookupNode(
          (targetAirport.node.get.latitude, targetAirport.node.get.longitude)
        )
      else
        println(s"Flight ${airplane.id} is being redirected to ${TQ}")
        val Q3 = lookupNode((-1, -11))
        val Q1 = lookupNode((1, 11))
        if TQ == 3 then Q3 else Q1

    println(
      s"Flight ${airplane.id} is ${distance(start, end)} points from target"
    )

    val steps = Seq.range[Int](0, distance(start, end).toInt)
    // How many steps in point space
    val psteps = steps.foldLeft(Seq(start)) { (points, step) =>
      // Each stop we should be able to close both x,y
      val nx =
        if points.last.x == end.x then points.last.x
        else if points.last.x < end.x then points.last.x + 1
        else points.last.x - 1
      val ny =
        if points.last.y == end.y then points.last.y
        else if points.last.y < end.y then points.last.y + 1
        else points.last.y - 1

      val candidate = Point(nx, ny)

      // lookupPoint.get(candidate).map(_.restricted).getOrElse(true)

      points :+ candidate
    }

    // Point space to routing grid nodes, drop visiting same node twice
    val nsteps = psteps.mapFilter(lookupPoint.get).distinct
    println(
      s"Flight ${airplane.id} will reach target in ${nsteps.length} steps"
    )

    nsteps
  }
