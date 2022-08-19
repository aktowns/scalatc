package planners.astar

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.syntax.*
import cats.implicits.*
import cats.mtl.*
import cats.effect.{IO, Sync}
import atc.v1 as atc
import cats.{ApplicativeError, Monad, Applicative}
import ui.v1 as ui

// A purely functional implementation of the A* path finding algorithm

class Graph(val nodes: Map[Position, Node]):
  def neighbours(node: Node): Seq[Node] = List(
    nodes.get(Position(node.row - 1, node.col)),
    nodes.get(Position(node.row + 1, node.col)),
    nodes.get(Position(node.row, node.col - 1)),
    nodes.get(Position(node.row, node.col + 1))
  ).flatten

  def render(
      grid: Map[Position, Boolean],
      s: Position,
      e: Position,
      path: List[Position],
      dist: Int
  ): String =

    val cols =
      (-dist to dist).map(col => col.toString.padTo(2, ' ')).mkString("")
    val rendered = (-dist to dist).map { row =>
      row.toString.padTo(2, ' ') +:
        (dist to (-dist, -1)).map { col =>
          Position(row, col) match
            case p if p == s           => 'S'
            case p if p == e           => 'E'
            case p if path.contains(p) => 'o'
            case p                     => grid.get(p).map(if (_) '#' else '.').getOrElse(' ')
        }
    }
    cols + "\n" + rendered.map(_.mkString(" ")).mkString("\n")

def path(node: Node): List[Node] =
  node.parent match
    case None         => Nil
    case Some(parent) => node :: path(parent)

def manhattan(pos1: Position, pos2: Position): Int =
  math.abs(pos2.x - pos1.x) + math.abs(pos2.y - pos1.y)

enum PlannerError:
  case AirportNotFound extends PlannerError
  case FlightPlanNonExisting extends PlannerError
  case AirportPositionNotFound extends PlannerError

extension [A](o: Option[A])
  def or[F[_]: Monad, Error](error: Error)(using err: Raise[F, Error]): F[A] =
    o.fold(err.raise(error))(Monad[F].pure)

/** This still isn't optimal, we feed the prior flights into the model to build up world which means plans lower in the
  * stack become more complex.
  *
  * We're given a point in pixel space we first lock down to a node then path find on the routing grid
  */
object AStarPlanner:
  def papply[F[_]: Sync: Logger](airplane: atc.Airplane, airports: Vector[atc.Airport], grid: Vector[ui.NodePoint])(
      using err: Raise[F, PlannerError]
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

  def search[F[_]: Logger](
      startPosition: Position,
      endPosition: Position,
      grid: Map[Position, Boolean]
  ): F[Vector[Position]] = ???

//   def apply(
//       airplane: atc.Airplane,
//       airports: Seq[atc.Airport],
//       grid: Seq[ui.NodePoint]
//   ): Seq[atc.Node] =
//     println(
//       s"Flight ${airplane.id} is looking to go to ${airplane.tag} airport"
//     )
//
//     val targetAirport = airports.find(_.tag == airplane.tag).get
//     val start = airplane.flightPlan.head // lookupPoint(airplane.point.get)
//     val end = targetAirport.node.get
//
//     println(s"Flight ${airplane.id} wants to go from ${start} to ${end}")
//
//     val grid2 = grid.map { np =>
//       (Position.fromModel(np.node.get), np.node.get.restricted)
//     }.toMap
//
//     val route = search(
//       Position.fromModel(start),
//       Position.fromModel(end),
//       grid2
//     )
//
//     println(
//       s"Flight ${airplane.id} route is ${route.length} steps starting at ${route.headOption}"
//     )
//
//     start +: route.map(p => atc.Node(-p.y, -p.x))
//
//   def search(
//       startPosition: Position,
//       endPosition: Position,
//       grid: Map[Position, Boolean]
//   ): Seq[Position] =
//     val nodes = grid.map { (pos, wall) => (pos, new Node(pos, wall)) }
//     val graph = new Graph(nodes.toMap)
//     val end = graph.nodes(endPosition)
//
//     val heap = new mutable.PriorityQueue[Node]()
//     heap.enqueue(graph.nodes(startPosition))
//
//     while (heap.nonEmpty)
//       // Grab the lowest f(x) to process next. Heap keeps this sorted for us.
//       val node = heap.dequeue
//
//       // End case -- result has been found, return the traced path
//       if (node == end) then
//         val p = path(node).reverse.map(_.pos)
//         println(graph.render(grid, startPosition, endPosition, p))
//         return p
//       else
//         // Normal case -- move node from open to closed, process each of its neighbors
//         node.closed = true
//         for (
//           neighbour <- graph.neighbours(node);
//           if (!neighbour.restricted && !neighbour.closed)
//         )
//           // g score is the shortest distance from start to current node, we need to check if
//           // the path we have arrived at this neighbor is the shortest one we have seen yet
//           val gScore = node.g + 1
//           val beenVisited = neighbour.visited
//
//           if !beenVisited || gScore < neighbour.g then
//             neighbour.parent = Some(node)
//             neighbour.update(gScore, manhattan(neighbour.pos, end.pos))
//             if !beenVisited
//             then // Pushing to heap will put it in proper place based on the 'f' value.
//               heap.enqueue(neighbour)
//     Nil
