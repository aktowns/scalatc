package planners.astar

import cats.implicits.*
import cats.*
import cats.effect.implicits.*
import cats.effect.{IO, Sync}
import atc.v1 as atc
import cats.{ApplicativeError, Monad, Applicative}
import ui.v1 as ui
import planners.{Planner, Position}
import cats.effect.std.PQueue

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

// def whileSome[M[_]: Monad, A, B](m: M[Option[A]], f: A => M[B]): M[B] =
//   for
//     a <- m
//     r <- a match
//            case Some(a) => f(a)
//            case None => ???
//   yield ???
//
//
//
// def foldWhileM[G[_], S](initial: S)(f: (S, A) => G[Option[S]])(implicit F: Foldable[F], G: Monad[G]): G[S] =
//   G.tailRecM((initial, FoldableStream.from(fa))) {
//     case (s, FoldableStream.Empty)      => G.pure(Right(s))
//     case (s, FoldableStream.Cons(h, t)) =>
//       G.map(f(s, h)) {
//         case None     => Right(s)
//         case Some(s1) => Left((s1, t.value))
//       }
//   }
//
//
// def whileDefined[M[_]: Monad, A, B](m: M[Option[A]], f: A => M[B]): M[B] =
//   m.flatMap {
//     case Some(a) => f(a) <* whileDefined(m, f)
//     case None => Monad[M].unit
//   }
//
// extension [F[_]: Monad, A](p: F[Option[A]])
//   def whileM[G[_], B](body: A => F[B])(implicit G: Alternative[G]): F[G[B]] = {
//      val b = Eval.later(body)
//      p.tailRecM[G[A], G[A]](G.empty[B])((xs: G[B]) =>
//        p.flatMap {
//         case Some(a) => b.value(a).map { bv => Left(G.appendK(xs, bv)) }
//         case None => p.pure(Right(xs))
//        }
//       //  ifM(p)(
//       //    ifTrue = {
//       //      map(b.value) { bv =>
//       //        Left(G.appendK(xs, bv))
//       //      }
//       //    },
//       //    ifFalse = pure(Right(xs))
//       //  )
//      )
//   }

object AStarPlanner extends Planner:
  import cats.effect.unsafe.implicits.global

  def search(
      startPosition: Position,
      endPosition: Position,
      grid: Map[Position, Boolean]
  ): Vector[Position] =
    val nodes = grid.map { (pos, wall) => (pos, Node.fromDefaults(pos, wall)) }
    val graph = new Graph(nodes.toMap)
    val end = graph.nodes(endPosition)

    for
      queue <- PQueue.unbounded[IO, Node]
      _ <- queue.offer(graph.nodes(startPosition))
      results <- queue.tryTake.flatMap {
        // Nothing left in the queue
        case None => IO(Some(Nil))
        // result has been found, return traced path
        case Some(node) if node == end =>
          IO(Some(path(node).reverse.map(_.pos)))
        // move node from open to closed, process each of its neighbors
        case Some(node) =>
          for
            _ <- queue.offer(node.copy(closed = true))
            res <- graph.neighbours(node).filter(neighbour => !neighbour.restricted && !neighbour.closed).traverse {
              neighbour =>
                val gScore = node.gScore + 1
                val beenVisited = neighbour.visited

                if !beenVisited || gScore < neighbour.g then
                  neighbour.parent = Some(node)
                  neighbour.update(gScore, manhattan(neighbour.pos, end.pos))
                  if !beenVisited then graph.offer(neighbour)
                  else IO(None)
                else IO(None)
            }
          yield res
      }.untilDefinedM
    yield ???

    ???

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
