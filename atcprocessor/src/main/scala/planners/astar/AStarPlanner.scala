package planners.astar

import cats.implicits.*
import cats.*
import cats.effect.implicits.*
import cats.effect.{Sync}
import atc.v1 as atc
import cats.{ApplicativeError, Monad, Applicative}
import ui.v1 as ui
import planners.{Planner, Position}
import cats.effect.std.PQueue
import cats.data.StateT
import org.typelevel.log4cats.Logger

case class Graph(nodes: Map[Position, Node]):
  def neighbours(node: Node): Vector[Node] = Vector(
    nodes.get(Position(node.row - 1, node.col)),
    nodes.get(Position(node.row + 1, node.col)),
    nodes.get(Position(node.row, node.col - 1)),
    nodes.get(Position(node.row, node.col + 1))
  ).flatten

  def updateNode(node: Node, f: Node => Node): Graph = ???

//  def render(
//      grid: Map[Position, Boolean],
//      s: Position,
//      e: Position,
//      path: List[Position],
//      dist: Int
//  ): String =
//
//    val cols =
//      (-dist to dist).map(col => col.toString.padTo(2, ' ')).mkString("")
//    val rendered = (-dist to dist).map { row =>
//      row.toString.padTo(2, ' ') +:
//        (dist to (-dist, -1)).map { col =>
//          Position(row, col) match
//            case p if p == s           => 'S'
//            case p if p == e           => 'E'
//            case p if path.contains(p) => 'o'
//            case p                     => grid.get(p).map(if (_) '#' else '.').getOrElse(' ')
//        }
//    }
//    cols + "\n" + rendered.map(_.mkString(" ")).mkString("\n")

def path(node: Node): Vector[Node] =
  node.parent match
    case None         => Vector.empty[Node]
    case Some(parent) => node +: path(parent)

def manhattan(pos1: Position, pos2: Position): Int =
  math.abs(pos2.x - pos1.x) + math.abs(pos2.y - pos1.y)

object AStarPlanner extends Planner:
  import cats.effect.unsafe.implicits.global

  type St[F[_], A] = StateT[F, Graph, A]

  private def searchEndCase[F[_]: Sync: Logger](pos: Position): St[F, Option[Vector[Position]]] =
    StateT.get.map { graph =>
      Some(path(graph.nodes(pos)).reverse.map(_.pos))
    }

  private def modifyNode[F[_]: Sync: Logger](pos: Position, modify: Node => Node): St[F, Node] = ???

  private def searchNormalCase[F[_]: Sync: Logger](
      pos: Position,
      endPosition: Position,
      queue: PQueue[F, Node]
  ): St[F, Option[Vector[Position]]] =
    for
      node <- modifyNode(pos, _.copy(closed = true))
      graph <- StateT.get[F, Graph]
      neighbours = graph.neighbours(node).filter(neighbour => !neighbour.restricted && !neighbour.closed)
      _ <- neighbours.traverse[St, Option[Vector[Position]]] { neighbour =>
        val gScore = node.gScore + 1
        val beenVisited = neighbour.visited

        if !beenVisited || gScore < neighbour.gScore then
          for
            neighbour <- modifyNode(neighbour.pos, _.copy(parent = Some(node)))
            distance = manhattan(neighbour.pos, endPosition)
            _ <- modifyNode(neighbour.pos, _.copy(parent = Some(node)).update(gScore, distance)).flatMap { neighbour =>
              if !beenVisited then StateT.lift(queue.offer(neighbour))
              else StateT.pure(())
            }
          yield None
        else StateT.pure[F, Graph, Option[Vector[Position]]](None)
      }
    yield None

  def search[F[_]: Sync: Logger](
      startPosition: Position,
      endPosition: Position,
      grid: Map[Position, Boolean]
  ): F[Vector[Position]] =
    val nodes = grid.map { (pos, wall) => (pos, Node.fromDefaults(pos, wall)) }
    val graph = Graph(nodes.toMap)

    val res = for
      queue <- StateT.lift[F, Graph, PQueue[F, Node]](PQueue.unbounded[F, Node])
      _ <- StateT.lift(queue.offer(graph.nodes(startPosition)))
      results <- StateT
        .lift(queue.tryTake)
        .flatMap {
          // Nothing left in the queue
          case None => StateT.pure[F, Graph, Option[Vector[Position]]](Some(Vector.empty))
          // result has been found, return traced path
          case Some(node) if node.pos == endPosition => searchEndCase(node.pos)
          // move node from open to closed, process each of its neighbors
          case Some(node) => searchNormalCase(node.pos, endPosition, queue)
        }
        .untilDefinedM
    yield results

    res.runA(graph)

  //  val heap = new mutable.PriorityQueue[Node]()
  //  heap.enqueue(graph.nodes(startPosition))

  //  while (heap.nonEmpty)
  //    // Grab the lowest f(x) to process next. Heap keeps this sorted for us.
  //    val node = heap.dequeue

  //    // End case -- result has been found, return the traced path
  //    if (node == end) then
  //      return path(node).reverse.map(_.pos)
  //    else
  //      // Normal case -- move node from open to closed, process each of its neighbors
  //      node.closed = true
  //      for (
  //        neighbour <- graph.neighbours(node);
  //        if (!neighbour.restricted && !neighbour.closed)
  //      )
  //        // g score is the shortest distance from start to current node, we need to check if
  //        // the path we have arrived at this neighbor is the shortest one we have seen yet
  //        val gScore = node.g + 1
  //        val beenVisited = neighbour.visited

  //        if !beenVisited || gScore < neighbour.g then
  //          neighbour.parent = Some(node)
  //          neighbour.update(gScore, manhattan(neighbour.pos, end.pos))
  //          if !beenVisited
  //          then // Pushing to heap will put it in proper place based on the 'f' value.
  //            heap.enqueue(neighbour)
  //  Nil
