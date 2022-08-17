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

import ui.v1.*
import atc.v1.Airplane
import atc.v1.Point as GamePoint
import atc.v1.Map as GameMap
import atc.v1.Node as GameNode

import io.grpc.ServerBuilder
import scalafx.Includes._
import scalafx.scene.text.Font
import scalafx.beans.value.ObservableValue
import scalafx.beans.Observable
import javafx.beans.property.ListProperty
import scalafx.beans.binding.Bindings
import scalafx.collections.ObservableBuffer
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.beans.property.ReadOnlyObjectProperty
import scalafx.scene.Node
import javafx.beans.value.ChangeListener

object AtcWindow extends JFXApp3:
  val UNIT_SIZE = 32
  val PADDING = 32
  val SCALING = 2

  val gameMap: ObjectProperty[Option[GameMap]] =
    ObjectProperty[Option[GameMap]](None)

  val nodePoints: ObjectProperty[Option[Map[GameNode, GamePoint]]] =
    ObjectProperty[Option[Map[GameNode, GamePoint]]](None)

  val gameState: ObjectProperty[Option[UIState]] =
    ObjectProperty[Option[UIState]](None)

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

    val rroutingGrid = createObjectBinding(
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
          new controls.ATCMap {
            mapWidth <== createDoubleBinding(
              () =>
                ((gameMap.value
                  .map(_.width)
                  .getOrElse(
                    1
                  ) * (UNIT_SIZE * SCALING)) + ((PADDING * SCALING) * 2)),
              gameMap
            )
            mapHeight = 100
            routingGrid = Seq(GameNode(1, 1))
          },
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
                      .getOrElse(
                        0
                      ) * (UNIT_SIZE * SCALING)) + ((PADDING * SCALING) * 2)),
                  gameMap
                )
                height <== createDoubleBinding(
                  () =>
                    ((gameMap.value
                      .map(_.height)
                      .getOrElse(
                        0
                      ) * (UNIT_SIZE * SCALING)) + ((PADDING * SCALING) * 2)),
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
                    s"Client connected, ${gameMap.value
                        .map(_.routingGrid.length)
                        .getOrElse(0)} nodes updated, ${gameState.value.map(_.planes.length).getOrElse(0)} planes tracked",
                  gameMap,
                  gameState
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
            rroutingGrid.onChange { (ob, old, ne) =>
              children.clear()
              children ++= (ne.flatMap { node =>
                val xOff = gameMap.value.map(_.width).getOrElse(0) / 2
                val yOff = gameMap.value.map(_.height).getOrElse(0) / 2

                val cx = xOff + node.longitude
                val cy = yOff - node.latitude

                val point = nodePoints.value.get(node)
                val px =
                  ((PADDING * SCALING) + (xOff * (UNIT_SIZE * SCALING))) + (point.x * SCALING)
                val py =
                  ((PADDING * SCALING) + (yOff * (UNIT_SIZE * SCALING))) - (point.y * SCALING)

                println(
                  s"latitude=${node.latitude} longitude=${node.longitude} cx=${cx} cy=${cy} restricted=${node.restricted} point=${point} px=${px} py=${py}"
                )

                Seq(
                  new Rectangle {
                    layoutX = px - (UNIT_SIZE / 2)
                    layoutY = py - (UNIT_SIZE / 2)
                    // layoutX = (cx * UNIT_SIZE) + UNIT_SIZE
                    // layoutY = (cy * UNIT_SIZE) + UNIT_SIZE
                    width = UNIT_SIZE
                    height = UNIT_SIZE
                    fill =
                      if node.restricted then Color.Red
                      else Color.rgb(20, 20, 20)
                  },
                  new Rectangle {
                    layoutX = px
                    layoutY = py
                    // layoutX = (cx * UNIT_SIZE) + UNIT_SIZE
                    // layoutY = (cy * UNIT_SIZE) + UNIT_SIZE
                    width = 2
                    height = 2
                    fill = Color.Gold
                  },
                  new Text {
                    layoutX =
                      px // (cx * UNIT_SIZE) + UNIT_SIZE + (UNIT_SIZE / 2) - 5
                    layoutY =
                      py - 5 // (cy * UNIT_SIZE) + UNIT_SIZE + (UNIT_SIZE / 2) - 5
                    text = s"${node.latitude}, ${node.longitude}"
                    font = Font.apply(10)
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
          },
          new Pane {
            gameState.onChange { (ob, old, ne: Option[UIState]) =>
              children = ne
                .map(
                  _.planes ++ Seq(
                    Airplane(
                      "AT-SANITY",
                      Some(
                        GamePoint(
                          gameMap.value
                            .map(_.width)
                            .getOrElse(0) / 2,
                          gameMap.value
                            .map(_.height)
                            .getOrElse(0) / 2
                        )
                      )
                    )
                  )
                )
                .getOrElse(Vector.empty[Airplane])
                .flatMap { plane =>
                  println(s"plane=${plane}")
                  val xOff = gameMap.value.map(_.width).getOrElse(0) / 2
                  val yOff = gameMap.value.map(_.height).getOrElse(0) / 2

                  val cx = (gameMap.value
                    .map(_.width)
                    .getOrElse(0) / 2) + plane.point.map(_.y).getOrElse(0)
                  val cy = (gameMap.value
                    .map(_.height)
                    .getOrElse(0) / 2) - plane.point.map(_.x).getOrElse(0)

                  val point = plane.point.get
                  val px = (PADDING + (xOff * UNIT_SIZE)) + point.x
                  val py = (PADDING + (yOff * UNIT_SIZE)) - point.y

                  Seq(
                    new Rectangle {
                      layoutX = px
                      layoutY = py
                      width = 4
                      height = 4
                      fill =
                        if plane.tag.isTagRed then Color.Orange
                        else Color.AliceBlue
                    },
                    new Text {
                      text = plane.id
                      layoutX = px
                      layoutY = py - 5
                      fill =
                        if plane.tag.isTagRed then Color.Orange
                        else Color.AliceBlue
                    }
                  )
                }
            }
          }
        )
      }
    }

  def onMapUpdate(): Unit =
    Platform.runLater { () =>
      gameMap.update(WindowState.gameMap.get())
    }

  def onNodePointUpdate(): Unit =
    Platform.runLater { () =>
      nodePoints.update(
        WindowState.nodePoints
          .get()
          .map(_.map(np => (np.node.get, np.point.get)).toMap)
      )
    }

  def onStateUpdate(): Unit =
    Platform.runLater { () =>
      gameState.update(WindowState.gameState.get())
    }
