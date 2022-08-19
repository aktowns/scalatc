import java.util.concurrent.atomic.AtomicReference

import ui.v1.*
import atc.v1.Point as GamePoint
import atc.v1.Map as GameMap
import atc.v1.Node as GameNode

object WindowState:
  val gameMap: AtomicReference[Option[GameMap]] =
    AtomicReference(None)
  val nodePoints: AtomicReference[Option[Seq[NodePoint]]] =
    AtomicReference(None)
  val proposedPoints: AtomicReference[Option[Seq[Seq[NodePoint]]]] =
    AtomicReference(None)
  val gameState: AtomicReference[Option[UIState]] =
    AtomicReference(None)

  def setGameMap(map: Option[GameMap]) =
    gameMap.set(map)
    AtcWindow.onMapUpdate()

  def setNodePoints(nps: Option[Seq[NodePoint]]) =
    nodePoints.set(nps)
    AtcWindow.onNodePointUpdate()

  def setProposedPoints(nps: Option[Seq[Seq[NodePoint]]]) =
    proposedPoints.set(nps)
    AtcWindow.onProposedPointUpdate()

  def setGameState(state: Option[UIState]) =
    gameState.set(state)
    AtcWindow.onStateUpdate()
