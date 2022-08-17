import ui.v1.*
import scala.concurrent.Future

class UIImpl(using ec: scala.concurrent.ExecutionContext)
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
