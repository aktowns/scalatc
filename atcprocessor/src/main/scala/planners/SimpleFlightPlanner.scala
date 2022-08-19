package planners

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

    val lookupPoint = grid.map(np => (np.point, np.node)).toMap
    val lookupNode: Map[(Int, Int), Point] =
      grid
        .map(np => ((np.node.latitude, np.node.longitude), np.point))
        .toMap
    val targetAirport = airports.find(_.tag == airplane.tag).getOrElse(Airport.defaultInstance)

    // val cX = lookupPoint.keys.minBy(v => math.abs(v.x - airplane.point.get.x))
    // val cY = lookupPoint.keys.minBy(v => math.abs(v.y - airplane.point.get.y))

    val start = airplane.point.getOrElse(Point.defaultInstance)
    val end = lookupNode(
      (
        targetAirport.node.getOrElse(Node.defaultInstance).latitude,
        targetAirport.node.getOrElse(Node.defaultInstance).longitude
      )
    )

    // val TQ = if airplane.tag.isTagBlue then 1 else 3
    // val end =
    //   if quadrant(start) == TQ then
    //     println(s"Flight ${airplane.id} is in ${TQ} going to airport")
    //     lookupNode(
    //       (targetAirport.node.get.latitude, targetAirport.node.get.longitude)
    //     )
    //   else
    //     println(s"Flight ${airplane.id} is being redirected to ${TQ}")
    //     val Q3 = lookupNode((-1, -11))
    //     val Q1 = lookupNode((1, 11))
    //     if TQ == 3 then Q3 else Q1

    println(
      s"Flight ${airplane.id} is ${distance(start, end)} points from target"
    )

    val steps = Seq.range[Int](0, distance(start, end).toInt)
    // How many steps in point space
    val psteps = steps.foldLeft(Seq(start)) { (points, step) =>
      // Each stop we should be able to close both x,y
      val (nx, ny) = points.lastOption
        .map { lastPoint =>
          val nx =
            if lastPoint.x == end.x then lastPoint.x
            else if lastPoint.x < end.x then lastPoint.x + 1
            else lastPoint.x - 1
          val ny =
            if lastPoint.y == end.y then lastPoint.y
            else if lastPoint.y < end.y then lastPoint.y + 1
            else lastPoint.y - 1
          (nx, ny)
        }
        .getOrElse((0, 0))
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
