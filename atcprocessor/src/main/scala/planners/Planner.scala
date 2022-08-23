package planners

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.syntax.*
import cats.implicits.*
import cats.effect.*
import cats.mtl.*

import atc.v1 as atc
import ui.v1 as ui

import utils.*

enum PlannerError:
  case AirportNotFound extends PlannerError
  case FlightPlanNonExisting extends PlannerError
  case AirportPositionNotFound extends PlannerError

trait Planner:
  def apply[F[_]: Sync: Logger](airplane: atc.Airplane, airports: Vector[atc.Airport], grid: Vector[ui.NodePoint])(using
      err: Raise[F, PlannerError]
  ): F[Vector[atc.Node]] =
    for
      _ <- info"Flight ${airplane.id} is looking to go to ${airplane.tag} airport"

      targetAirport <- airports.find(_.tag == airplane.tag).or(PlannerError.AirportNotFound)
      startPosition <- airplane.flightPlan.headOption.or(PlannerError.FlightPlanNonExisting)
      endPosition <- targetAirport.node.or(PlannerError.AirportPositionNotFound)

      _ <- info"Flight ${airplane.id} wants to go from ${startPosition} to ${endPosition}"

      normalizedGrid = grid.map((np: ui.NodePoint) => (Position.fromModel(np.node), np.node.restricted)).toMap

      route <- search(Position.fromModel(startPosition), Position.fromModel(endPosition), normalizedGrid)

      _ <- info"Flight ${airplane.id} route is ${route.length}"
    yield startPosition +: route.map(p => atc.Node(-p.y, -p.x, false))

  def search[F[_]: Sync: Logger](
      startPosition: Position,
      endPosition: Position,
      grid: Map[Position, Boolean]
  ): F[Vector[Position]]
