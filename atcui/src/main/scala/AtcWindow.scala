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
import scalafx.Includes.*
import scalafx.scene.text.Font
import scalafx.beans.value.ObservableValue
import scalafx.beans.Observable
import javafx.beans.property.ListProperty
import scalafx.beans.binding.{Bindings, ObjectBinding}
import scalafx.collections.ObservableBuffer
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.beans.property.ReadOnlyObjectProperty
import scalafx.scene.Node
import javafx.beans.value.ChangeListener
import cats.implicits.*

object AtcWindow extends JFXApp3:
  val gameMap: ObjectProperty[Option[GameMap]] =
    ObjectProperty[Option[GameMap]](None)

  val nodePoints: ObjectProperty[Option[Map[GameNode, GamePoint]]] =
    ObjectProperty[Option[Map[GameNode, GamePoint]]](None)

  val gameState: ObjectProperty[Option[UIState]] =
    ObjectProperty[Option[UIState]](None)

  override def start(): Unit =
    Platform.runLater {
      UIServiceImpl.runServer
    }

    gameMap.onChange((_, _, _) => stage.sizeToScene())

    val routingGridInp = createObjectBinding(
      () =>
        gameMap.value
          .flatMap { nmap =>
            nmap.routingGrid.traverse { node =>
              val maybePoint = nodePoints.value.flatMap(_.get(node))
              maybePoint.map(point => (node, point))
            }
          }
          .getOrElse(Seq.empty),
      gameMap,
      nodePoints
    )

    val airportsInp = createObjectBinding(
      () =>
        gameMap.value
          .flatMap { nmap =>
            nmap.airports.traverse { airport =>
              val maybePoint = nodePoints.value.flatMap(points =>
                airport.node.flatMap(points.get)
              )
              maybePoint.map(point => (airport, point))
            }
          }
          .getOrElse(Seq.empty),
      gameMap,
      nodePoints
    )

    val airplanesInp = createObjectBinding(
      () => gameState.value.map(_.planes).getOrElse(Seq.empty),
      gameState
    )

    val hasMap = createBooleanBinding(
      () => gameMap.value.isDefined,
      gameMap
    )

    val mapWidthInp = createDoubleBinding(
      () => gameMap.value.map(_.width).getOrElse(1),
      gameMap
    )

    val mapHeightInp = createDoubleBinding(
      () => gameMap.value.map(_.height).getOrElse(1),
      gameMap
    )

    val statusStrInp = createStringBinding(
      { () =>
        val gridLen =
          gameMap.value.map(_.routingGrid.length).getOrElse(0)
        val planes =
          gameState.value.map(_.planes.length).getOrElse(0)
        s"gRPC UI Server port 50051, Client connected, ${gridLen} nodes updated, ${planes} planes tracked, using SimpleFlightPlanner solver"
      },
      gameMap,
      gameState
    )

    stage = new JFXApp3.PrimaryStage {
      title.value = "Scalatc Viewer"
      resizable = false
      scene = new Scene {
        fill = Color.rgb(38, 38, 38)
        content = Seq(
          new Pane {
            visible <== hasMap
            children = Seq(
              new controls.ATCMap {
                mapWidth <== mapWidthInp
                mapHeight <== mapHeightInp
                routingGrid <== routingGridInp
                airports <== airportsInp
                airplanes <== airplanesInp
                unitSize = 32
                scaling = 2
              },
              new Text {
                text <== statusStrInp
                fill = Color.White
                layoutX = 10
                layoutY = 20
              }
            )
          },
          new Pane {
            visible <== !hasMap
            children = Seq(
              new Text {
                text = "Waiting for client connection.."
                font = Font.apply(30)
                fill = Color.White
                layoutX = 30
                layoutY = 30
              }
            )
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
