import model._

import scala.collection.immutable.::

object MyStrategy {
  val hockeyistZones:Map[Long, List[Zone]] = Map.empty

  private def activeHockeyist(hockeyist:Hockeyist) = hockeyist.hokeyistType != HockeyistType.Goalie &&
    hockeyist.state != HockeyistState.KnockedDown &&
    hockeyist.state != HockeyistState.Resting
  private def teammate(hockeyist:Hockeyist) = hockeyist.teammate
  private def opponent(hockeyist:Hockeyist) = !hockeyist.teammate
  
  def activeHockeyists(w:World)(kind: Hockeyist => Boolean) = w.hockeyists.collect {
    case hockeyist if kind(hockeyist) && activeHockeyist(hockeyist) => hockeyist
  }

  private def nearest(u:Unit, zones: List[Zone]):Zone = {
    zones.minBy(z => math.hypot(u.x - z.center._1, u.y - z.center._2))
  }

  private def getNearest(x: Double, y: Double, world: World)(kind: Hockeyist => Boolean): Option[Hockeyist] = {
    val hockeyists = activeHockeyists(world)(kind)
    if (hockeyists.isEmpty) {
      None
    } else {
      Some(hockeyists.minBy { hockeyist => math.hypot(x - hockeyist.x, y - hockeyist.y)})
    }
  }
  
  def opponentsInRadius(x: Double, y: Double, world: World, radius:Double): Seq[Hockeyist] = {
    val hockeyists = activeHockeyists(world)(opponent)
    hockeyists.filter(hockeyist => hockeyist.distanceTo(x, y) <= radius)
  }

  def canPassTo(unit:model.Unit, x: Double, y: Double) = {

  }

  private def getNearestOpponent(x: Double, y: Double, world: World): Option[Hockeyist] = getNearest(x, y, world)(opponent)
  private def getNearestOpponent(u:model.Unit, world: World): Option[Hockeyist] = getNearest(u.x, u.y, world)(opponent)
  private def getNearestTeammate(x: Double, y: Double, world: World): Option[Hockeyist] = getNearest(x, y, world)(teammate)
  private def getNearestTeammate(u:model.Unit, world: World): Option[Hockeyist] = getNearest(u.x, u.y, world)(teammate)
  private def playerGoalie(player:Player, world: World):Option[Hockeyist] = world.hockeyists
    .collectFirst{case h if h.hokeyistType == HockeyistType.Goalie && h.playerId == player.id => h}
  private[MyStrategy] final val StrikeAngle = 1.0D * math.Pi / 180.0D

  val mainZone = new Zone(165d, 1035d, 150d, 770d)
  val subzones = mainZone.divide(8, 5)
  def detectZone(u:Unit) = subzones.values.collectFirst{case z if z.contains(u.x, u.y, u.radius) => z}
  val upperZone = subzones((2,1))
  val centerZone = subzones((3, 1))
  val lowerZone = subzones((3, 1))

  val leftGlobalUp = new Zone(65d, 1135d, 150d, 460d)
  val leftGlobalBottom = new Zone(65d, 1135d, 460d, 770d)
  val rightGlobalUp = new Zone(65d, 1135d, 150d, 460d)
  val rightGlobalBottom = new Zone(65d, 1135d, 460d, 770d)

  private lazy val leftTopZoneStart = new Zone(600d, 900d, 200d, 490d)
  private lazy val leftTopZoneMiddle = new Zone(491.25, 600d, 210d, 400d)
  private lazy val leftTopZoneFinish = new Zone(382.5, 491.25, 274.0, 398.0, true)

  private lazy val leftBottomZoneStart = new Zone(600d, 900d, 430d, 720d)
  private lazy val leftBottomZoneMiddle = new Zone(491.25, 600d, 520d, 710d)
  private lazy val leftBottomZoneFinish = new Zone(382.5, 491.25, 522d, 646.0, true)

  private lazy val leftTopZone = new Zone(65d, 491.25, 150d, 460d)
  private lazy val leftTopStrikeable = new Zone(260d, 491.25, 150d, 460d, true)
  private lazy val leftBottomZone = new Zone(65d, 491.25, 460d, 770d)
  private lazy val leftBottomStrikeable = new Zone(260d, 491.25, 460d, 770d, true)

  private def rightMirror(z:Zone) = new Zone(1200 - z.right, 1200 - z.left, z.top, z.bottom)

  private lazy val rightTopZoneStart = rightMirror(leftTopZoneStart)
  private lazy val rightTopZoneMiddle = rightMirror(leftTopZoneMiddle)
  private lazy val rightTopZoneFinish = rightMirror(leftTopZoneFinish)

  private lazy val rightBottomZoneStart = rightMirror(leftBottomZoneStart)
  private lazy val rightBottomZoneMiddle = rightMirror(leftBottomZoneMiddle)
  private lazy val rightBottomZoneFinish = rightMirror(leftBottomZoneFinish)

  private lazy val rightTopZone = rightMirror(leftTopZone)
  private lazy val rightTopStrikeable = rightMirror(leftTopStrikeable)
  private lazy val rightBottomZone = rightMirror(leftBottomZone)
  private lazy val rightBottomStrikeable = rightMirror(leftBottomStrikeable)

  def topZoneStart(enemyAtLeft:Boolean) = if (enemyAtLeft) leftTopZoneStart else rightTopZoneStart
  def topZoneMiddle(enemyAtLeft:Boolean) = if (enemyAtLeft) leftTopZoneMiddle else rightTopZoneMiddle
  def topZoneFinish(enemyAtLeft:Boolean) = if (enemyAtLeft) leftTopZoneFinish else rightTopZoneFinish
  def bottomZoneStart(enemyAtLeft:Boolean) = if (enemyAtLeft) leftBottomZoneStart else rightBottomZoneStart
  def bottomZoneMiddle(enemyAtLeft:Boolean) = if (enemyAtLeft) leftBottomZoneMiddle else rightBottomZoneMiddle
  def bottomZoneFinish(enemyAtLeft:Boolean) = if (enemyAtLeft) leftBottomZoneFinish else rightBottomZoneFinish
  def topZone(enemyAtLeft:Boolean) = if (enemyAtLeft) leftTopZone else rightTopZone
  def topStrikeable(enemyAtLeft:Boolean) = if (enemyAtLeft) leftTopStrikeable else rightTopStrikeable
  def bottomZone(enemyAtLeft:Boolean) = if (enemyAtLeft) leftBottomZone else rightBottomZone
  def bottomStrikeable(enemyAtLeft:Boolean) = if (enemyAtLeft) leftBottomStrikeable else rightBottomStrikeable

  def globalUp(enemyAtLeft:Boolean) = if (enemyAtLeft) leftGlobalUp else rightGlobalUp
  def globalBottom(enemyAtLeft:Boolean) = if (enemyAtLeft) leftGlobalBottom else rightGlobalBottom  
  ////
  leftGlobalUp.neighbours= List(leftTopStrikeable, leftBottomZoneStart)
  leftGlobalBottom.neighbours= List(leftBottomStrikeable, leftTopZoneStart)

  leftTopZoneStart.neighbours= List(leftTopZoneMiddle, leftBottomZoneStart)
  leftTopZoneMiddle.neighbours= List(leftTopZoneFinish)

  leftBottomZoneStart.neighbours= List(leftBottomZoneMiddle, leftTopZoneStart)
  leftBottomZoneMiddle.neighbours= List(leftBottomZoneFinish)

  leftTopZone.neighbours= List(leftBottomZone, leftTopZoneStart)
  leftTopStrikeable.neighbours= List(leftTopZone)

  leftBottomZone.neighbours= List(leftTopZone, leftBottomZoneStart)
  leftBottomStrikeable.neighbours= List(leftBottomZone)
  
  ////

  rightGlobalUp.neighbours= List(rightTopStrikeable, rightBottomZoneStart)
  rightGlobalBottom.neighbours= List(rightBottomStrikeable, rightTopZoneStart)

  rightTopZoneStart.neighbours= List(rightTopZoneMiddle, rightBottomZoneStart)
  rightTopZoneMiddle.neighbours= List(rightTopZoneFinish)

  rightBottomZoneStart.neighbours= List(rightBottomZoneMiddle, rightTopZoneStart)
  rightBottomZoneMiddle.neighbours= List(rightBottomZoneFinish)

  rightTopZone.neighbours= List(rightBottomZone, rightTopZoneStart)
  rightTopStrikeable.neighbours= List(rightTopZone)

  rightBottomZone.neighbours= List(rightTopZone, rightBottomZoneStart)
  rightBottomStrikeable.neighbours= List(rightBottomZone)


  val defenceLeftNet = new Zone(130d, 230d, 430d, 490d)
  val defenceRightNet = rightMirror(defenceLeftNet)//new Zone(1030d, 1090d, 430d, 490d)
  //val defenceLeftNet = rightMirror(defenceRightNet)

  def defence(enemyAtLeft:Boolean) = if (enemyAtLeft) defenceRightNet else defenceLeftNet
   //val targetLowerZone = new Zone(400d, 500d, 530d, 620d)
  val defence1 = new Zone(1165d, 1125d, 370d, 430d)
  val defence3 = new Zone(1165d, 1125d, 490d, 550d)

  //println(subzones)
}

trait Rectangle {
  def left: Double
  def right: Double
  def top:Double
  def bottom:Double
  def middleX:Double = (left + right) / 2
  def middleY:Double = (top + bottom) / 2
  def center:(Double, Double) = (middleX, middleY)

  def contains(x:Double, y:Double, raduis:Double):Boolean = x >= left + raduis && x <= right - raduis && y >=top + raduis && y <= bottom - raduis
  def contains(u:Unit):Boolean = contains(u.x, u.y, u.radius)
  def containsCenter(u:Unit):Boolean = contains(u.x, u.y,0)
  def intersectsP(u:Unit) = contains(u.x, u.y, -u.radius)

  override def toString: String = s"($left, $right, $top, $bottom)"
}

class Zone(val left: Double, val right : Double, val top:Double, val bottom:Double,
           /*neighbours: => List[Zone] = List.empty,*/ strikeable:Boolean = false) extends Rectangle {
  var neighbours: List[Zone] = List.empty
  def divide(xSides:Int, ySides:Int):Map[(Int, Int), Zone] = {
    val xLength = (right - left) / xSides
    val yLength = (bottom - top) / ySides
    val zones = for (xZoneNum <- 0 until xSides; yZoneNum <- 0 until ySides) yield (xZoneNum, yZoneNum) -> subzone(left, xLength, xZoneNum, top,yLength, yZoneNum)
    zones.toMap
  }

  private def subzone(left: Double, leftSize:Double, leftZoneNum:Int, top:Double, topSize:Double, topZoneNum:Int):Zone = {
    val xOffset = leftZoneNum*leftSize
    val yOffset = topZoneNum*topSize
    new Zone (left + xOffset, left+xOffset+leftSize, top + yOffset, top+yOffset+topSize)
  }

  def mostConvinientNeighbourTo(u:model.Unit):Option[Zone] = {
    if (neighbours.isEmpty) None
    else Some(Zone.mostConvinient(neighbours)(u))
  }

}

object Zone {
  def mostConvinient(zones:List[Zone])(u:Unit) = zones.minBy(z => math.abs(u.angleTo(z.center._1, z.center._2)))

}

class MyStrategy extends Strategy {
  import MyStrategy._
  def move(self: Hockeyist, world: World, game: Game, move: Move) = {
    val zones= hockeyistZones.get(self.id).toList.flatten
    val currentZone = detectZone(self)
    val updatedZones = zones match {
      case p::pp::Nil if p != currentZone && pp != currentZone => currentZone :: p :: Nil
      case p :: pp :: Nil => zones
      case _ => currentZone :: zones
    }
    hockeyistZones.updated(self.id, updatedZones)
    zones.contains(upperZone)

    /*if (self.teammateIndex == 1) {
      defenceCenter(self, move, world)
      /*if(globalUp.contains(world.puck))
        moveBottom(self, move, world)
      else moveUp(self, move, world)*/
    } else {*/

      self.state match {
        case HockeyistState.Swinging => move.action = ActionType.Strike
        case _ =>
          if (world.puck.ownerPlayerId.contains(self.playerId)) {
            if (world.puck.ownerHockeyistId.contains(self.id)) {
              drivePuckToHook(self, world, game, move)
            } else {
              //moveToSubzone(self, move)
              strikeNearestOpponent(self, world, game, move)
            }
          } else if (getNearestTeammate(world.puck, world).map(_.id).contains(self.id)) {
            moveToPuck(self, world.puck, move)
          } else {
            //moveToPuck(self, world.puck, move)

            strikeNearestOpponent(self, world, game, move)
          }
      }
    //}
  }

  private def strikeNearestOpponent(self: Hockeyist, world: World, game: Game, move: Move) {
    for (nearestOpponent <- getNearestOpponent(self.x, self.y, world)) {
      //println(nearestOpponent.teammate)
      if (self.distanceTo(nearestOpponent) > game.stickLength) {
        move.speedUp = 1.0D
        move.turn = self.angleTo(nearestOpponent)
      }
      if (math.abs(self.angleTo(nearestOpponent)) < 0.5D * game.stickSector) {
        move.action = ActionType.Strike
      }
    }
  }

  private def moveTo(self: Hockeyist, x:Double, y:Double, move: Move) {
    move.speedUp = 1.0D
    move.turn = self.angleTo(x, y)
    move.action = ActionType.TakePuck
  }

  def defenceCenter(self: Hockeyist, move: Move, w:World) = {
    move.action = ActionType.TakePuck
    val net = new Net(w.opponentPlayer.get, w)
    val myDefence = defence(net.netAtLeft)
    //println(defence(net.netAtLeft))
    if (myDefence.contains(self)) {
      move.speedUp = 0.0D
      move.turn = self.angleTo(w.puck)
    } else {
      move.speedUp = 0.2D
      move.turn = self.angleTo(myDefence.center._1, myDefence.center._2)
    }
  }
  def moveBottom(self: Hockeyist, move: Move, w:World) = {
    //println("to bottom")
    if (defenceRightNet.contains(self)) {
      move.speedUp = 0.3D
      move.turn = self.angleTo(defence3.center._1, defence3.center._2)
    } else if (defence3.contains(self)) {
      move.speedUp = 0.0D
      move.turn = self.angleTo(w.puck)
    } else {
      move.speedUp = 0.9D
      move.turn = self.angleTo(defenceRightNet.center._1, defenceRightNet.center._2)
    }
    move.action = ActionType.TakePuck

  }
  def moveUp(self: Hockeyist, move: Move, w:World) = {
    //println("to up")
    if (defenceRightNet.contains(self)) {
      move.speedUp = 0.3D
      move.turn = self.angleTo(defence1.center._1, defence1.center._2)
    } else if (defence1.contains(self)) {
      move.speedUp = 0.0D
      move.turn = self.angleTo(w.puck)
    } else {
      move.speedUp = 0.9D
      move.turn = self.angleTo(defenceRightNet.center._1, defenceRightNet.center._2)
    }
    move.action = ActionType.TakePuck
  }

  val up = (0, -30)
  val down = (0, 30)
  val left = (-30, 0)
  val right = (30, 0)

  def minus(source:(Double, Double), target:(Double, Double)) = (source._1 - target._1, target._2 - target._2)

  //def lefter(source:(Double, Double), target:(Double, Double))

  def calculateDirection(self: Hockeyist, world: World) = {
    val xy = (self.x, self.y)
    val nearbyenemies = opponentsInRadius(self.x, self.y, world, 80).map(h => (h.x, h.y))

  }


  private def moveToPuck(self: Hockeyist, puck: Puck, move: Move) = moveTo(self, puck.x, puck.y, move)

  private def drivePuck(self: Hockeyist, world: World, game: Game, move: Move) {
    val Some((netX, netY)) = for {
      opponentPlayer <- world.opponentPlayer
      netX = 0.5D * (opponentPlayer.netBack + opponentPlayer.netFront)
      netY = {
        val ny = 0.5D * (opponentPlayer.netBottom + opponentPlayer.netTop)
        ny + ((if (self.y < ny) 0.5D else -0.5D) * game.goalNetHeight)
      }
    } yield (netX, netY)

    val angleToNet = self.angleTo(netX, netY)
    move.turn = angleToNet
    if (math.abs(angleToNet) < StrikeAngle) {
      move.action = ActionType.Swing
    }
  }

  /*private def operateWithTarget(net:Net, target:(Double, Double), self: Hockeyist, world: World, game: Game, move: Move) = {
    val angleToTarget = self.angleTo(target._1, target._2)
    val turned = turnedToAttack(self, angleToTarget, net.oppositeNet)
    if (leftTopStrikeable.containsCenter(self)) {
      if (turned) {
        move.turn = angleToTarget
        if (math.abs(angleToTarget) < StrikeAngle) {
          move.action = ActionType.Swing
        }
      } else {

      }
    }
  }*/

  def turnedToAttack(self: Hockeyist, angleToTarget:Double, myNet:Net) = {
    math.abs(angleToTarget) < math.abs(self.angleTo(myNet.center._1, myNet.center._2))
  }
  private def drivePuckToHook(self: Hockeyist, world: World, game: Game, move: Move) {
    val net = new Net(world.opponentPlayer.get, world)

    val netAtLeft = net.netAtLeft

    if (globalUp(netAtLeft).containsCenter(self)) {//мы наверху
      //println("up")
      val target = net.targetBottom
      val angleToTarget = self.angleTo(target._1, target._2)
      val turned = turnedToAttack(self, angleToTarget, net.oppositeNet)
      if (turned) {
        //println("turned")
        if (topStrikeable(netAtLeft).containsCenter(self)) {
          //println("to strike")
          move.turn = angleToTarget
          if (math.abs(angleToTarget) < StrikeAngle) {
            move.action = ActionType.Swing
          }
        } else if (topZoneStart(netAtLeft).containsCenter(self)) {
          //println("at zone 2")
          moveToSubzone(self, move, topZoneStart(netAtLeft).mostConvinientNeighbourTo(self).get)
        } else if (topZoneMiddle(netAtLeft).containsCenter(self)) {
          //println("at zone 1")
          moveToSubzone(self, move, topZoneMiddle(netAtLeft).mostConvinientNeighbourTo(self).get)
        } else {
          //println("at top zone")
          moveToSubzone(self, move, globalUp(netAtLeft).mostConvinientNeighbourTo(self).get)
        }
      } else {
        //println("not turned")
        moveToSubzone(self, move, Zone.mostConvinient(globalBottom(netAtLeft) :: topZoneStart(netAtLeft) :: Nil )(self))
      }
    } else { //мы внизу
      //println("down")
      val target = net.targetTop
      val angleToTarget = self.angleTo(target._1, target._2)
      val turned = turnedToAttack(self, angleToTarget, net.oppositeNet)

      if (turned) {
        //println("turned")
        if (bottomStrikeable(netAtLeft).containsCenter(self)) {
          move.turn = angleToTarget
          if (math.abs(angleToTarget) < StrikeAngle) {
            move.action = ActionType.Swing
          }
        } else if (bottomZoneStart(netAtLeft).containsCenter(self)) {
          moveToSubzone(self, move, bottomZoneStart(netAtLeft).mostConvinientNeighbourTo(self).get)
        } else if (bottomZoneMiddle(netAtLeft).containsCenter(self)) {
          moveToSubzone(self, move, bottomZoneMiddle(netAtLeft).mostConvinientNeighbourTo(self).get)
        } else {
          moveToSubzone(self, move, globalBottom(netAtLeft).mostConvinientNeighbourTo(self).get)
        }

      } else {
        //println("not turned")
        moveToSubzone(self, move,  Zone.mostConvinient(globalUp(netAtLeft) :: bottomZoneStart(netAtLeft) :: Nil )(self))
      }

    }
    //println("")

  }

  def moveToSubzone(self: Hockeyist, move: Move, zone:Zone) = {
    moveTo(self, zone.center._1, zone.center._2, move)
  }

  class Net(player:Player, world: World) extends Rectangle {
    def top:Double = player.netTop
    def bottom:Double = player.netBottom
    def left:Double = player.netLeft
    def right:Double = player.netRight
    def targetBottom:(Double, Double) = if (netAtLeft) (right - 15, bottom)  else (left + 15, bottom)
    def targetTop:(Double, Double) = if (netAtLeft) (right - 15, top)  else (left + 15, top)

    def oppositeNet = world.players.collectFirst{case opponent if opponent.id != player.id => new Net(opponent, world)}.get

    def goalie:Option[Hockeyist] = playerGoalie(player, world)

    def goalieAtTop = goalie.map(g => (g.y - g.radius) == top)
    def goalieAtBottom = goalie.map(g => (g.y + g.radius) == bottom)

    val goalieToTop = goalie.map(g => Math.abs(g.y - top))
    val goalieToBottom = goalie.map(g => Math.abs(g.y - bottom))

    def guardedSide = for {
        toTop <- goalieToTop
        toBottom <- goalieToBottom
      } yield guardedSideInt(toTop,toBottom)
    def guardedSideInt(toTop:Double, toBottom:Double) = if (toTop <= toBottom) top else bottom

    def unguardedSideInt(toTop:Double, toBottom:Double) = if(toTop > toBottom) top + world.puck.radius  else bottom - world.puck.radius
    def unguardedSide = for {
        toTop <- goalieToTop
        toBottom <- goalieToBottom
      } yield unguardedSideInt(toTop, toBottom)


    def goalieAtMargin = for {
      t <- goalieAtTop
      b <- goalieAtBottom
    } yield t || b

    def netAtLeft = player.netFront == right
    def netAtRight = !netAtLeft

    def strikeableX = player.netBack + 20//if (netAtLeft) right - world.puck.radius else left + world.puck.radius
    val goalieSize = goalie.map(_.radius).getOrElse(0.0) * 2
    def goalieLine = if(netAtLeft) right + goalieSize else left - goalieSize
  }
}
