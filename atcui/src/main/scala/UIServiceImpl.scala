import ui.v1.*
import scala.concurrent.Future
import io.grpc.ServerBuilder

class UIServiceImpl(using ec: scala.concurrent.ExecutionContext)
    extends UIServiceGrpc.UIService:
  override def setMap(request: SetMapRequest): Future[SetMapResponse] =
    println("Updating map")
    WindowState.setNodePoints(Some(request.lookup))
    WindowState.setGameMap(request.map)
    Future.successful(SetMapResponse.defaultInstance)

  override def setUIState(
      request: SetUIStateRequest
  ): Future[SetUIStateResponse] =
    WindowState.setGameState(request.state)
    Future.successful(SetUIStateResponse.defaultInstance)

object UIServiceImpl:
  def runServer: Unit =
    implicit val ec: scala.concurrent.ExecutionContext =
      scala.concurrent.ExecutionContext.global

    val server = ServerBuilder
      .forPort(50051)
      .addService(UIServiceGrpc.bindService(new UIServiceImpl, ec))
      .build
      .start

    println("Server started, listening on 50051")
