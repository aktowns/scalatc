import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder
import fs2.grpc.syntax.all.*
import cats.effect.*
import cats.implicits.*
import io.grpc.ManagedChannel
import io.grpc.Metadata
import io.grpc.netty.shaded.io.netty.channel.ChannelOption
import atc.v1.game.*
import atc.v1.atc.*
import atc.v1.event.*
import atc.v1.map.*
import atc.v1.airplane.*
import ui.v1.ui.*
import monocle.syntax.all.*

case class Services[F[_]](
    atcService: AtcServiceFs2Grpc[F, Metadata],
    gameService: GameServiceFs2Grpc[F, Metadata],
    eventService: EventServiceFs2Grpc[F, Metadata],
    mapService: MapServiceFs2Grpc[F, Metadata],
    airplaneService: AirplaneServiceFs2Grpc[F, Metadata],
    uiService: UIServiceFs2Grpc[F, Metadata]
)

extension (services: Services[IO])
  def getVersion: IO[Option[Version]] =
    services.atcService
      .getVersion(
        GetVersionRequest.defaultInstance,
        new Metadata()
      )
      .map(_.version)

  def getGameState: IO[GetGameStateResponse.GameState] =
    services.gameService
      .getGameState(
        GetGameStateRequest.defaultInstance,
        new Metadata()
      )
      .map(_.gameState)

  def startGame: IO[Unit] =
    services.gameService
      .startGame(
        StartGameRequest.defaultInstance,
        new Metadata()
      )
      .void

  def getMap: IO[Option[Map]] =
    services.mapService
      .getMap(
        GetMapRequest.defaultInstance,
        new Metadata()
      )
      .map(_.map)

  def setMap(gameMap: Map): IO[Unit] =
    services.uiService
      .setMap(
        SetMapRequest(Some(gameMap)),
        new Metadata()
      )
      .void

  def setState(state: UIState): IO[Unit] =
    services.uiService
      .setUIState(
        SetUIStateRequest(Some(state)),
        new Metadata()
      )
      .void

  def getAirplane(id: String): IO[Option[Airplane]] =
    services.airplaneService
      .getAirplane(
        GetAirplaneRequest(id),
        new Metadata()
      )
      .map(_.airplane)

  def eventStream: fs2.Stream[IO, StreamResponse.Event] =
    services.eventService
      .stream(
        StreamRequest.defaultInstance,
        new Metadata()
      )
      .map(_.event)

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
        IO { println(value); state }
      case StreamResponse.Event.LandingAborted(value) =>
        IO { println(value); state }

  def runProgram(services: Services[IO]): IO[Unit] = for {
    version <- services.getVersion
    gameMap <- services.getMap.map(_.get)
    _ <- services.setMap(gameMap)
    _ <- services.startGame
    evStream <- services.eventStream
      .evalScan(State.empty)((state, event) =>
        processEvents(services, event, state)
      )
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
