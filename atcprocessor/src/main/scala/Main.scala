import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder
import io.grpc.netty.shaded.io.netty.channel.ChannelOption
import fs2.grpc.syntax.all.*
import cats.effect.*
import cats.implicits.*
import io.grpc.{ManagedChannel, Metadata}
import monocle.syntax.all.*
import scala.concurrent.duration.*
import cats.Order

import atc.v1.*
import ui.v1.*

case class State(planes: scala.collection.immutable.Map[String, Airplane])
object State:
  def empty: State = State(scala.collection.immutable.Map.empty)

object Main extends IOApp.Simple:
  def processEvents(
      services: Services[IO],
      event: StreamResponse.Event,
      state: State
  ): IO[State] =
    event match
      case StreamResponse.Event.Empty => IO.pure(state)
      case StreamResponse.Event.GameStarted(value) =>
        IO { println("Game started"); state }
      case StreamResponse.Event.GameStopped(value) =>
        IO { println("Game stopped"); state }
      case StreamResponse.Event.AirplaneCollided(value) =>
        IO { println(value); state }
      case StreamResponse.Event.AirplaneDetected(value) =>
        IO {
          val plane = value.airplane.get
          state.focus(_.planes).modify(_ + (plane.id -> plane))
        }
      case StreamResponse.Event.AirplaneLanded(value) =>
        IO {
          state.focus(_.planes.at(value.id)).replace(None)
        }
      case StreamResponse.Event.AirplaneMoved(value) =>
        IO {
          state
            .focus(_.planes.index(value.id).as[Airplane].point)
            .replace(value.point)
        }
      case StreamResponse.Event.FlightPlanUpdated(value) =>
        IO {
          state
            .focus(_.planes.index(value.id).as[Airplane].flightPlan)
            .replace(value.flightPlan)
        }
      case StreamResponse.Event.LandingAborted(value) =>
        IO { println(value); state }

  def simpleFlightPlanner(
      airplane: Airplane,
      airports: Seq[Airport],
      grid: Seq[NodePoint]
  ): IO[Seq[Node]] = IO {
    println(
      s"Flight ${airplane.id} is looking to go to ${airplane.tag} airport"
    )
    val lookupPoint = grid.map(np => (np.point.get, np.node.get)).toMap
    val lookupNode = grid.map(np => (np.node.get, np.point.get)).toMap
    val targetAirport = airports.find(_.tag == airplane.tag).get

    val cX = lookupPoint.keys.minBy(v => math.abs(v.x - airplane.point.get.x))
    val cY = lookupPoint.keys.minBy(v => math.abs(v.y - airplane.point.get.y))

    val start = airplane.point.get
    val end = lookupNode(targetAirport.node.get)

    Seq.range(start, end)(pointOrd).map(lookupPoint)
  }

  def runProgram(services: Services[IO]): IO[Unit] = for {
    version <- services.getVersion
    gameMap <- services.getMap.map(_.get)
    nps <- gameMap.routingGrid
      .traverse { node =>
        services.nodeToPoint(node).map(point => NodePoint(Some(node), point))
      }
    _ <- services.setMap(gameMap, nps)
    _ <- services.startGame
    evStream <- services.eventStream
      .evalScan(State.empty)((state, event) =>
        processEvents(services, event, state)
      )
      .debounce(500.milliseconds)
      .evalTap { state =>
        services.setState(UIState(state.planes.values.toSeq))
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
