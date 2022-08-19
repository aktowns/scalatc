package controls

import javafx.scene.layout.{Border, Pane as JFXPane}
import scalafx.scene.layout.Pane as SFXPane
import atc.v1.{Airplane, Airport, Node as GameNode, Point as GamePoint}
import javafx.beans.property.{DoubleProperty, SimpleDoubleProperty}
import javafx.beans.property.{ObjectProperty, SimpleObjectProperty}
import javafx.scene.shape.{Line, Rectangle}
import javafx.beans.binding.{Bindings, DoubleBinding}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.scene.paint.{Color, Paint}
import javafx.scene.text.{Font, Text}

import java.lang
import java.util.concurrent.Callable
import cats.implicits.*
import ui.v1.NodePoint

import scala.language.implicitConversions

object ATCMap:
  implicit def sfxATCMap2jfx(v: ATCMap): ATCMapJFX = v.delegate

@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
class ATCMap(override val delegate: ATCMapJFX = new ATCMapJFX) extends SFXPane(delegate):

  def mapWidth: DoubleProperty = delegate.mapWidth

  def mapWidth_=(width: Double): Unit = delegate.mapWidth.set(width)

  def mapHeight: DoubleProperty = delegate.mapHeight

  def mapHeight_=(height: Double): Unit = delegate.mapHeight.set(height)

  def routingGrid: ObjectProperty[Seq[(GameNode, GamePoint)]] =
    delegate.routingGrid

  def routingGrid_=(routing: Seq[(GameNode, GamePoint)]): Unit =
    delegate.routingGrid.set(routing)

  def airports: ObjectProperty[Seq[(Airport, GamePoint)]] = delegate.airports

  def airports_=(airports: Seq[(Airport, GamePoint)]): Unit =
    delegate.airports.set(airports)

  def airplanes: ObjectProperty[Seq[Airplane]] = delegate.airplanes

  def airplanes_=(airplanes: Seq[Airplane]): Unit =
    delegate.airplanes.set(airplanes)

  def proposed: ObjectProperty[Seq[Seq[NodePoint]]] = delegate.proposed

  def proposed_=(proposed: Seq[Seq[NodePoint]]): Unit =
    delegate.proposed.set(proposed)

  def unitSize: DoubleProperty = delegate.unitSize

  def unitSize_=(size: Double): Unit = delegate.unitSize.set(size)

  def scaling: DoubleProperty = delegate.scaling

  def scaling_=(scaling: Double): Unit = delegate.scaling.set(scaling)

class ATCMapJFX extends JFXPane:
  val PADDING = 32

  val mapWidth: DoubleProperty = new SimpleDoubleProperty(0)
  val mapHeight: DoubleProperty = new SimpleDoubleProperty(0)
  val routingGrid: ObjectProperty[Seq[(GameNode, GamePoint)]] =
    new SimpleObjectProperty[Seq[(GameNode, GamePoint)]](
      Seq.empty[(GameNode, GamePoint)]
    )
  val airports: ObjectProperty[Seq[(Airport, GamePoint)]] =
    new SimpleObjectProperty[Seq[(Airport, GamePoint)]](
      Seq.empty[(Airport, GamePoint)]
    )
  val airplanes: ObjectProperty[Seq[Airplane]] =
    new SimpleObjectProperty[Seq[Airplane]](Seq.empty[Airplane])
  val proposed: ObjectProperty[Seq[Seq[NodePoint]]] =
    new SimpleObjectProperty[Seq[Seq[NodePoint]]](Seq.empty[Seq[NodePoint]])
  val unitSize: DoubleProperty = new SimpleDoubleProperty(32)
  val scaling: DoubleProperty = new SimpleDoubleProperty(2)

  val calcMapWidth = Bindings.createDoubleBinding(
    () => mapWidth.getValue * (unitSize.getValue * scaling.getValue) + ((PADDING * scaling.getValue) * 2),
    mapWidth,
    scaling,
    unitSize
  )

  val calcMapHeight = Bindings.createDoubleBinding(
    () => mapHeight.getValue * (unitSize.getValue * scaling.getValue) + ((PADDING * scaling.getValue) * 2),
    mapHeight,
    scaling,
    unitSize
  )

  val calcUnitSize = Bindings.createDoubleBinding(
    () => unitSize.getValue * scaling.getValue,
    unitSize,
    scaling
  )

  val xOff: DoubleBinding = Bindings.createDoubleBinding(
    () => ((mapWidth.getValue / 2) * (unitSize.getValue * scaling.getValue)),
    mapWidth,
    unitSize,
    scaling
  )
  val yOff: DoubleBinding = Bindings.createDoubleBinding(
    () => ((mapHeight.getValue / 2) * (unitSize.getValue * scaling.getValue)),
    mapWidth,
    unitSize,
    scaling
  )

  routingGrid.addListener(new ChangeListener[Seq[(GameNode, GamePoint)]] {
    override def changed(
        observableValue: ObservableValue[_ <: Seq[(GameNode, GamePoint)]],
        oldNodes: Seq[(GameNode, GamePoint)],
        newNodes: Seq[(GameNode, GamePoint)]
    ): Unit =
      val nodes = newNodes.map { (node, point) =>
        val px = Bindings.createDoubleBinding(
          () => xOff.getValue + (point.x * scaling.getValue),
          scaling,
          xOff
        )
        val py = Bindings.createDoubleBinding(
          () => yOff.getValue - (point.y * scaling.getValue),
          scaling,
          yOff
        )

        println(
          s"latitude=${node.latitude} longitude=${node.longitude} restricted=${node.restricted} point=${point} px=${px} py=${py}"
        )

        val gridSq = new Rectangle()
        gridSq.layoutXProperty().bind(px.subtract(calcUnitSize.divide(2)))
        gridSq.layoutYProperty().bind(py.subtract(calcUnitSize.divide(2)))
        gridSq.widthProperty().bind(calcUnitSize)
        gridSq.heightProperty().bind(calcUnitSize)
        gridSq.setFill(
          if node.restricted then Color.ORANGE else Color.rgb(20, 20, 20)
        )
        gridSq.setOpacity(if node.restricted then 0.3 else 1.0)

        val gridDot = new Rectangle()
        gridDot.layoutXProperty().bind(px)
        gridDot.layoutYProperty().bind(py)
        gridDot.setWidth(2)
        gridDot.setHeight(2)
        gridDot.setFill(Color.GOLD)

        val textColour =
          (node.latitude == 0 || node.longitude == 0, node.restricted) match
            case (true, true)   => Color.DARKGREEN
            case (true, false)  => Color.LIGHTGREEN
            case (false, true)  => Color.BLACK
            case (false, false) => Color.GRAY

        val gridText = new Text()
        gridText.layoutXProperty().bind(px.subtract(2))
        gridText.layoutYProperty().bind(py.subtract(2))
        gridText.setText(s"${node.latitude}, ${node.longitude}")
        gridText.setFont(Font.font(14))
        gridText.setFill(textColour)

        val sq = new JFXPane()
        sq.getChildren.setAll(gridSq, gridDot, gridText)
        sq
      }

      baseGrid.getChildren.clear()
      baseGrid.getChildren.setAll(nodes: _*)
  })

  airports.addListener(new ChangeListener[Seq[(Airport, GamePoint)]] {
    override def changed(
        observableValue: ObservableValue[_ <: Seq[(Airport, GamePoint)]],
        oldAirports: Seq[(Airport, GamePoint)],
        newAirports: Seq[(Airport, GamePoint)]
    ): Unit =
      val nodes = newAirports.map { (airport, point) =>
        val px = Bindings.createDoubleBinding(
          () => xOff.getValue + (point.x * scaling.getValue),
          scaling,
          xOff
        )
        val py = Bindings.createDoubleBinding(
          () => yOff.getValue - (point.y * scaling.getValue),
          scaling,
          yOff
        )

        println(
          s"airport latitude=${airport.node.map(_.latitude)} longitude=${airport.node
              .map(_.longitude)} px=${px} py=${py}"
        )

        val gridSq = new Rectangle()
        gridSq.layoutXProperty().bind(px.subtract(calcUnitSize.divide(2)))
        gridSq.layoutYProperty().bind(py.subtract(calcUnitSize.divide(2)))
        gridSq.widthProperty().bind(calcUnitSize)
        gridSq.heightProperty().bind(calcUnitSize)
        gridSq.setFill(
          if airport.tag.isTagRed then Color.LIGHTPINK else Color.LIGHTSKYBLUE
        )
        gridSq
      }

      airportGrid.getChildren.clear()
      airportGrid.getChildren.setAll(nodes: _*)
  })

  airplanes.addListener(new ChangeListener[Seq[Airplane]] {
    override def changed(
        observableValue: ObservableValue[_ <: Seq[Airplane]],
        oldAirplanes: Seq[Airplane],
        newAirplanes: Seq[Airplane]
    ): Unit =
      val nodes = newAirplanes.map { airplane =>
        val lookup = routingGrid.getValue.toMap

        val px = Bindings.createDoubleBinding(
          () => xOff.getValue + (airplane.point.fold(0)(_.x) * scaling.getValue),
          scaling,
          xOff
        )
        val py = Bindings.createDoubleBinding(
          () => yOff.getValue - (airplane.point.fold(0)(_.y) * scaling.getValue),
          scaling,
          yOff
        )

        val gridDot = new Rectangle()
        gridDot.layoutXProperty().bind(px.subtract(5))
        gridDot.layoutYProperty().bind(py.subtract(5))
        gridDot.setWidth(10)
        gridDot.setHeight(10)
        gridDot.setFill(
          if (airplane.tag.isTagRed) then Color.LIGHTPINK
          else Color.LIGHTSKYBLUE
        )

        val gridText = new Text()
        gridText.layoutXProperty().bind(px.subtract(2))
        gridText.layoutYProperty().bind(py.subtract(2))
        gridText.setText(s"${airplane.id}")
        gridText.setFont(Font.font(20))
        gridText.setFill(
          if (airplane.tag.isTagRed) then Color.LIGHTPINK
          else Color.LIGHTSKYBLUE
        )

        val path = airplane.flightPlan.mapFilter(lookup.get)

        val joinedPath: Seq[(GamePoint, GamePoint)] =
          path.drop(1).foldLeft(Seq((airplane.point.get, path.head))) { (prev, next) =>
            prev.lastOption.map(_._2) match
              case Some(p) => prev :+ (p, next)
              case None    => prev
          }

        val paths = joinedPath.map { (start, end) =>
          val pStartX = xOff.getValue + (start.x * scaling.getValue)
          val pStartY = yOff.getValue - (start.y * scaling.getValue)
          val pEndX = xOff.getValue + (end.x * scaling.getValue)
          val pEndY = yOff.getValue - (end.y * scaling.getValue)

          val pathLine = new Line()
          pathLine.setFill(
            if (airplane.tag.isTagRed) then Color.LIGHTPINK
            else Color.LIGHTSKYBLUE
          )
          pathLine.setStroke(
            if (airplane.tag.isTagRed) then Color.LIGHTPINK
            else Color.LIGHTSKYBLUE
          )
          pathLine.setStrokeWidth(3)
          pathLine.setStartX(pStartX)
          pathLine.setStartY(pStartY)
          pathLine.setEndX(pEndX)
          pathLine.setEndY(pEndY)
          pathLine
        }

        val sq = new JFXPane()
        sq.getChildren.setAll(Seq(gridDot, gridText) ++ paths: _*)
        sq
      }

      airplaneGrid.getChildren.clear()
      airplaneGrid.getChildren.setAll(nodes: _*)
  })

  proposed.addListener(new ChangeListener[Seq[Seq[NodePoint]]] {
    override def changed(
        observableValue: ObservableValue[_ <: Seq[Seq[NodePoint]]],
        oldProposed: Seq[Seq[NodePoint]],
        newProposed: Seq[Seq[NodePoint]]
    ): Unit =
      val nodes = newProposed.map { proposed =>

        val joinedPath: Seq[(GamePoint, GamePoint)] = proposed
          .drop(1)
          .foldLeft(
            Seq((proposed.head.point, proposed.head.point))
          ) { (prev, next) =>
            prev.lastOption.map(_._2) match
              case Some(p) => prev :+ (p, next.point)
              case None    => prev
          }

        val paths = joinedPath.map { (start, end) =>
          val pStartX = xOff.getValue + (start.x * scaling.getValue)
          val pStartY = yOff.getValue - (start.y * scaling.getValue)
          val pEndX = xOff.getValue + (end.x * scaling.getValue)
          val pEndY = yOff.getValue - (end.y * scaling.getValue)

          val pathLine = new Line()
          pathLine.setFill(Color.MAGENTA)
          pathLine.setStroke(Color.MAGENTA)
          pathLine.setStrokeWidth(1)
          pathLine.setStartX(pStartX)
          pathLine.setStartY(pStartY)
          pathLine.setEndX(pEndX)
          pathLine.setEndY(pEndY)
          pathLine.setOpacity(0.8)
          pathLine
        }

        val sq = new JFXPane()
        sq.getChildren.setAll(paths: _*)
        sq
      }

      proposedGrid.getChildren.clear()
      proposedGrid.getChildren.setAll(nodes: _*)

  })

  private val mapRect: Rectangle = new Rectangle()
  private val baseGrid: JFXPane = new JFXPane()
  private val airportGrid: JFXPane = new JFXPane()
  private val airplaneGrid: JFXPane = new JFXPane()
  private val proposedGrid: JFXPane = new JFXPane()

  {
    mapRect.widthProperty().bind(calcMapWidth)
    mapRect.heightProperty().bind(calcMapHeight)
    mapRect.setFill(Color.BLACK)

    mapRect.setLayoutX(0)
    mapRect.setLayoutY(0)
    baseGrid.setLayoutX(PADDING * scaling.getValue)
    baseGrid.setLayoutY(PADDING * scaling.getValue)
    // baseGrid.setBorder(Border.stroke(Color.AQUA))
    airportGrid.setLayoutX(PADDING * scaling.getValue)
    airportGrid.setLayoutY(PADDING * scaling.getValue)
    // airportGrid.setBorder(Border.stroke(Color.MAGENTA))
    airplaneGrid.setLayoutX(PADDING * scaling.getValue)
    airplaneGrid.setLayoutY(PADDING * scaling.getValue)
    // airplaneGrid.setBorder(Border.stroke(Color.TOMATO))

    proposedGrid.setLayoutX(PADDING * scaling.getValue)
    proposedGrid.setLayoutY(PADDING * scaling.getValue)

    getChildren.addAll(
      mapRect,
      baseGrid,
      airportGrid,
      airplaneGrid,
      proposedGrid
    )
  }
