package planners

import cats.effect.IO
import atc.v1.{Airplane, Airport, Node, Point}
import ui.v1.NodePoint
import cats.{Order, Monad}
import cats.implicits.*

import cats.effect.Sync
import org.typelevel.log4cats.Logger

// |---------(+)----------
// |    2     |     1
// |          |
// (-)-----(0, 0)-------(+)
// |          |
// |    3     |     4
// |---------(-)----------
//
// Very basic planner, try to go from plane location directly to the target airport
object SimpleFlightPlanner extends Planner:

  override def search[F[_]: Sync: Logger](
      startPosition: Position,
      endPosition: Position,
      grid: Map[Position, Boolean]
  ): F[Vector[Position]] =
    val steps = Seq.range[Int](0, distance(startPosition, endPosition).toInt)

    val psteps = steps.foldLeft(Seq(startPosition)) { (points, step) =>
      // Each stop we should be able to close both x,y
      val (nx, ny) = points.lastOption
        .map { lastPoint =>
          val nx =
            if lastPoint.x == endPosition.x then lastPoint.x
            else if lastPoint.x < endPosition.x then lastPoint.x + 1
            else lastPoint.x - 1
          val ny =
            if lastPoint.y == endPosition.y then lastPoint.y
            else if lastPoint.y < endPosition.y then lastPoint.y + 1
            else lastPoint.y - 1
          (nx, ny)
        }
        .getOrElse((0, 0))

      points :+ Position(nx, ny)
    }

    Monad[F].pure(psteps.distinct.toVector)

  // def quadrant(a: Point): Int =
  //   if a.x > 0 && a.y > 0 then 1
  //   else if a.x > 0 && a.y < 0 then 4
  //   else if a.x < 0 && a.y > 0 then 2
  //   else if a.x < 0 && a.y < 0 then 3
  //   else 0

  def distance(a: Position, b: Position): Double =
    math.sqrt(math.pow(b.x - a.x, 2) + math.pow(b.y - a.y, 2) * 1.0)
