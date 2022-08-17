package controls

import javafx.scene.layout.Pane as JFXPane
import scalafx.scene.layout.Pane as SFXPane
import atc.v1.{Airplane, Airport, Node as GameNode}
import javafx.beans.property.{DoubleProperty, SimpleDoubleProperty}
import javafx.beans.property.{ObjectProperty, SimpleObjectProperty}
import javafx.scene.shape.Rectangle
import javafx.beans.binding.Bindings
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.scene.paint.Paint
import scalafx.scene.paint.Color

object ATCMap:
  implicit def sfxATCMap2jfx(v: ATCMap): ATCMapJFX = v.delegate

class ATCMap(override val delegate: ATCMapJFX = new ATCMapJFX)
    extends SFXPane(delegate):

  def mapWidth: DoubleProperty = delegate.mapWidth
  def mapWidth_=(width: Double): Unit = delegate.mapWidth.set(width)
  def mapHeight: DoubleProperty = delegate.mapHeight
  def mapHeight_=(height: Double): Unit = delegate.mapHeight.set(height)
  def routingGrid: ObjectProperty[Seq[GameNode]] = delegate.routingGrid
  def routingGrid_=(routing: Seq[GameNode]): Unit = delegate.routingGrid.set(routing)
  def airports: ObjectProperty[Seq[Airport]] = delegate.airports
  def airports_=(airports: Seq[Airport]): Unit = delegate.airports.set(airports)
  def airplanes: ObjectProperty[Seq[Airplane]] = delegate.airplanes
  def airplanes_=(airplanes: Seq[Airplane]): Unit = delegate.airplanes.set(airplanes)

class ATCMapJFX extends JFXPane:
  val mapWidth: DoubleProperty = new SimpleDoubleProperty(0)
  val mapHeight: DoubleProperty = new SimpleDoubleProperty(0)
  val routingGrid: ObjectProperty[Seq[GameNode]] = new SimpleObjectProperty[Seq[GameNode]](Seq.empty[GameNode])
  val airports: ObjectProperty[Seq[Airport]] = new SimpleObjectProperty[Seq[Airport]](Seq.empty[Airport])
  val airplanes: ObjectProperty[Seq[Airplane]] = new SimpleObjectProperty[Seq[Airplane]](Seq.empty[Airplane])

  routingGrid.addListener(new ChangeListener[Seq[GameNode]] {
    override def changed(observableValue: ObservableValue[_ <: Seq[GameNode]],
                         oldNodes: Seq[GameNode],
                         newNodes: Seq[GameNode]): Unit =
      val nodes = newNodes.map { node =>
        val rect = new Rectangle()
        rect.widthProperty().bind(unitSize)
        rect.heightProperty().bind(unitSize)
        rect.setFill(Color.ForestGreen)
        rect
      }

      baseGrid.getChildren.clear()
      baseGrid.getChildren.setAll(nodes: _*)

      println(nodes)
  })

  airports.addListener(new ChangeListener[Seq[Airport]] {
    override def changed(observableValue: ObservableValue[_ <: Seq[Airport]],
                         oldAirports: Seq[Airport],
                         newAirports: Seq[Airport]): Unit =
      val nodes = newAirports.map { airport =>
        val rect = new Rectangle()
        rect.widthProperty().bind(unitSize)
        rect.heightProperty().bind(unitSize)
        rect.setFill(Color.ForestGreen)
        rect
      }

      airportGrid.getChildren.clear()
      airportGrid.getChildren.setAll(nodes: _*)
  })

  airplanes.addListener(new ChangeListener[Seq[Airplane]] {
    override def changed(observableValue: ObservableValue[_ <: Seq[Airplane]],
                         oldAirplanes: Seq[Airplane],
                         newAirplanes: Seq[Airplane]): Unit =
      val nodes = newAirplanes.map { airplane =>
        val rect = new Rectangle()
        rect.widthProperty().bind(unitSize)
        rect.heightProperty().bind(unitSize)
        rect.setFill(Color.ForestGreen)
        rect
      }

      airplaneGrid.getChildren.clear()
      airplaneGrid.getChildren.setAll(nodes: _*)
  })

  val unitSize: DoubleProperty = new SimpleDoubleProperty(32)

  private val mapRect: Rectangle = new Rectangle()
  private val baseGrid: JFXPane = new JFXPane()
  private val airportGrid: JFXPane = new JFXPane()
  private val airplaneGrid: JFXPane = new JFXPane()

  {
    mapRect.widthProperty().bind(mapWidth)
    mapRect.heightProperty().bind(mapHeight)
    mapRect.setFill(Color.CadetBlue)

    getChildren.addAll(mapRect, baseGrid, airportGrid, airplaneGrid)
  }
