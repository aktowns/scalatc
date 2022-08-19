import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder
import io.grpc.netty.shaded.io.netty.channel.ChannelOption
import fs2.grpc.syntax.all.*
import cats.effect.*
import cats.implicits.*
import io.grpc.{ManagedChannel, Metadata}
import monocle.syntax.all.*
import scala.concurrent.duration.*

import atc.v1.*
import ui.v1.*

case class State(
    planes: scala.collection.immutable.Map[String, Airplane],
    isRunning: Boolean
)
object State:
  def empty: State = State(scala.collection.immutable.Map.empty, true)

object Main extends IOApp.Simple:
  private def processEvents(state: State, event: StreamResponse.Event): State =
    event match
      case StreamResponse.Event.Empty                   => state
      case StreamResponse.Event.GameStarted(value)      => state.focus(_.isRunning).replace(true)
      case StreamResponse.Event.GameStopped(value)      => state.focus(_.isRunning).replace(false)
      case StreamResponse.Event.AirplaneCollided(value) => state.focus(_.isRunning).replace(false)
      case StreamResponse.Event.AirplaneDetected(value) =>
        value.airplane match
          case Some(plane) => state.focus(_.planes).modify(_ + (plane.id -> plane))
          case None        => state
      case StreamResponse.Event.AirplaneLanded(value) => state.focus(_.planes.at(value.id)).replace(None)
      case StreamResponse.Event.AirplaneMoved(value) =>
        state
          .focus(_.planes.index(value.id).as[Airplane].point)
          .replace(value.point)
      case StreamResponse.Event.FlightPlanUpdated(value) =>
        state
          .focus(_.planes.index(value.id).as[Airplane].flightPlan)
          .replace(value.flightPlan)
      case StreamResponse.Event.LandingAborted(value) => state

  def runProgram(services: Services[IO]): IO[Unit] = for {
    version <- services.getVersion
    gameMap <- services.getMap.flatMap(_.fold(IO.raiseError(new RuntimeException("Failed to get map")))(_.pure))
    nps <- gameMap.routingGrid
      .traverse { node =>
        services.nodeToPoint(node).map { maybePoint =>
          maybePoint.map(point => NodePoint(node, point))
        }
      }
    _ <- services.setMap(gameMap, nps.flatten)
    _ <- services.startGame
    evStream <- services.eventStream
      .scan(State.empty)(processEvents)
      .debounce(100.milliseconds)
      .evalMap { state =>
        if state.isRunning then
          state.planes.values.toSeq
            .traverse { plane =>
              planners.SimpleFlightPlanner(plane, gameMap.airports, nps.flatten).flatMap { plan =>
                services.updateFlightPlan(plane.id, plan).flatTap { res => IO { println(res) } } *>
                  plan.traverse(n => services.nodeToPoint(n).map(point => NodePoint(n, point.getOrElse(Point(0, 0)))))
              }
            }
            .map(prop => (state, prop))
        else IO.pure(state, Seq.empty[Seq[NodePoint]])
      }
      .evalTap { (state, proposed) =>
        services.setState(UIState(state.planes.values.toSeq), proposed)
      }
      .compile
      .drain
  } yield evStream

  def run: IO[Unit] =
    val managedWorldChannelResource: Resource[IO, ManagedChannel] =
      NettyChannelBuilder
        .forAddress("127.0.0.1", 4747)
        .usePlaintext()
        .resource[IO]

    val managedUIChannelResource: Resource[IO, ManagedChannel] =
      NettyChannelBuilder
        .forAddress("127.0.0.1", 50051)
        .usePlaintext()
        .resource[IO]

    managedWorldChannelResource
      .flatMap { worldCh =>
        for
          atc <- AtcServiceFs2Grpc.stubResource[IO](worldCh)
          game <- GameServiceFs2Grpc.stubResource[IO](worldCh)
          event <- EventServiceFs2Grpc.stubResource[IO](worldCh)
          map <- MapServiceFs2Grpc.stubResource[IO](worldCh)
          airplane <- AirplaneServiceFs2Grpc.stubResource[IO](worldCh)
          uiCh <- managedUIChannelResource
          ui <- UIServiceFs2Grpc.stubResource[IO](uiCh)
        yield Services[IO](atc, game, event, map, airplane, ui)
      }
      .use(runProgram)
