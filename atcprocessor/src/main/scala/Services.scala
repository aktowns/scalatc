import atc.v1.*
import ui.v1.*
import io.grpc.Metadata

import cats.implicits.*
import cats.effect.IO

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

  def nodeToPoint(node: Node): IO[Option[Point]] =
    services.mapService
      .nodeToPoint(NodeToPointRequest(Some(node)), new Metadata())
      .map(_.point)

  def setMap(gameMap: Map, nodePoints: Seq[NodePoint]): IO[Unit] =
    services.uiService
      .setMap(
        SetMapRequest(Some(gameMap), nodePoints),
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

  def updateFlightPlan(id: String, flightPlan: Seq[Node]): IO[Unit] =
    services.airplaneService
      .updateFlightPlan(
        UpdateFlightPlanRequest(id, flightPlan),
        new Metadata()
      )
      .void

  def eventStream: fs2.Stream[IO, StreamResponse.Event] =
    services.eventService
      .stream(
        StreamRequest.defaultInstance,
        new Metadata()
      )
      .map(_.event)
