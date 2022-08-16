import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color.*
import scalafx.scene.paint.*
import scalafx.scene.text.Text
import scalafx.scene.shape.Rectangle
import scalafx.scene.shape.Circle
import scalafx.scene.layout.Pane
import scalafx.application.Platform

import ui.v1.ui.*
import scala.concurrent.Future

import java.util.concurrent.Semaphore
import java.util.concurrent.atomic.AtomicReference
import io.grpc.ServerBuilder
import scalafx.Includes._
import scalafx.scene.text.Font
import scalafx.beans.value.ObservableValue
import scalafx.beans.Observable
import javafx.beans.property.ListProperty
import scalafx.beans.binding.Bindings
import scalafx.collections.ObservableBuffer
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import cats.Functor
import scalafx.beans.property.ReadOnlyObjectProperty
import scalafx.scene.Node
import javafx.beans.value.ChangeListener

case class UIState()
case class Config(gameMap: atc.v1.map.Map)

object UIState:
  val gameMap: AtomicReference[Option[atc.v1.map.Map]] =
    AtomicReference(None)
  val gameState: AtomicReference[Option[ui.v1.ui.UIState]] =
    AtomicReference(None)

  def setGameMap(map: Option[atc.v1.map.Map]) =
    gameMap.set(map)
    AtcWindow.onMapUpdate()

class UIImpl(using ec: scala.concurrent.ExecutionContext)
    extends UIServiceGrpc.UIService:
  override def setMap(request: SetMapRequest): Future[SetMapResponse] =
    println("Updating map")
    UIState.setGameMap(request.map)
    Future.successful(SetMapResponse.defaultInstance)

  override def setUIState(
      request: SetUIStateRequest
  ): Future[SetUIStateResponse] =
    println("Updating GameState")
    UIState.gameState.set(request.state)
    Future.successful(SetUIStateResponse.defaultInstance)

object AtcWindow extends JFXApp3:
  val UNIT_SIZE = 50

  val gameMap: ObjectProperty[Option[atc.v1.map.Map]] =
    ObjectProperty[Option[atc.v1.map.Map]](None)

  val nodes: ObservableBuffer[Node] = ObservableBuffer()

  override def start(): Unit =
    Platform.runLater {
      implicit val ec: scala.concurrent.ExecutionContext =
        scala.concurrent.ExecutionContext.global

      val server = ServerBuilder
        .forPort(50051)
        .addService(UIServiceGrpc.bindService(new UIImpl, ec))
        .build
        .start

      println("Server started, listening on 50051")
    }

    val routingGrid = createObjectBinding(
      () => gameMap.value.map(_.routingGrid).getOrElse(Vector.empty),
      gameMap
    )

    gameMap.onChange((_, _, _) => stage.sizeToScene())

    stage = new JFXApp3.PrimaryStage {
      title.value = "Scalatc Viewer"
      resizable = false
      scene = new Scene {
        fill = Color.rgb(38, 38, 38)
        content = Seq(
          new Pane {
            children = Seq(
              new Text {
                text = "Waiting for client connection.."
                font = Font.apply(30)
                fill = Color.White
                layoutX = 30
                layoutY = 30
                visible <== createBooleanBinding(
                  () => gameMap.value.isEmpty,
                  gameMap
                )
              },
              new Rectangle {
                width <== createDoubleBinding(
                  () =>
                    ((gameMap.value
                      .map(_.width)
                      .getOrElse(0) * UNIT_SIZE) + (UNIT_SIZE * 2)),
                  gameMap
                )
                height <== createDoubleBinding(
                  () =>
                    ((gameMap.value
                      .map(_.height)
                      .getOrElse(0) * UNIT_SIZE) + (UNIT_SIZE * 2)),
                  gameMap
                )
                visible <== createBooleanBinding(
                  () => gameMap.value.isDefined,
                  gameMap
                )
              },
              new Text {
                text <== createStringBinding(
                  () =>
                    s"Client connected, ${gameMap.value.map(_.routingGrid.length).getOrElse(0)} nodes updated",
                  gameMap
                )
                fill = Color.White
                layoutX = 10
                layoutY = 20
                visible <== createBooleanBinding(
                  () => gameMap.value.isDefined,
                  gameMap
                )
              }
            )
          },
          new Pane {
            routingGrid.onChange { (ob, old, ne) =>
              children.clear()
              children ++= (ne.flatMap { node =>
                val cx =
                  (gameMap.value.map(_.width).getOrElse(0) / 2) + node.longitude
                val cy =
                  (gameMap.value.map(_.height).getOrElse(0) / 2) - node.latitude
                println(
                  s"latitude=${node.latitude} longitude=${node.longitude} cx=${cx} cy=${cy} restricted=${node.restricted}"
                )

                Seq(
                  new Rectangle {
                    layoutX = (cx * UNIT_SIZE) + UNIT_SIZE
                    layoutY = (cy * UNIT_SIZE) + UNIT_SIZE
                    width = UNIT_SIZE - 1
                    height = UNIT_SIZE - 1
                    fill =
                      if node.restricted then Color.Red
                      else Color.rgb(20, 20, 20)
                  },
                  new Text {
                    layoutX = (cx * UNIT_SIZE) + UNIT_SIZE + (UNIT_SIZE / 2) - 5
                    layoutY = (cy * UNIT_SIZE) + UNIT_SIZE + (UNIT_SIZE / 2) - 5
                    text = s"${node.latitude}, ${node.longitude}"

                    fill =
                      if (node.latitude == 0 || node.longitude == 0) then
                        Color.LightGreen
                      else Color.Gray
                  }
                )
              })
              val airports =
                gameMap.value.map(_.airports).getOrElse(Vector.empty)

              children ++= airports.map { airport =>
                val cx =
                  (gameMap.value.map(_.width).getOrElse(0) / 2) + airport.node
                    .map(_.longitude)
                    .getOrElse(0)
                val cy =
                  (gameMap.value.map(_.height).getOrElse(0) / 2) - airport.node
                    .map(_.latitude)
                    .getOrElse(0)

                new Rectangle {
                  layoutX = (cx * UNIT_SIZE) + UNIT_SIZE
                  layoutY = (cy * UNIT_SIZE) + UNIT_SIZE
                  width = UNIT_SIZE - 1
                  height = UNIT_SIZE - 1
                  fill =
                    if airport.tag.isTagRed then Color.Orange
                    else Color.Blue
                }
              }
            }
          }
        )
      }
    }

  def onMapUpdate(): Unit =
    Platform.runLater { () =>
      gameMap.update(UIState.gameMap.get())
    }
