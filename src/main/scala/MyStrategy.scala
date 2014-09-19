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
   val targetUpperZone = new Zone(382.5, 491.25, 274.0, 398.0)
   val uZone1 = new Zone(491.25, 600d, 250d, 440d)
   val uZone2 = new Zone(600d, 900d, 200d, 490d)
   val targetLowerZone = new Zone(400d, 500d, 530d, 620d)
  val defence1 = new Zone(1165d, 1125d, 370d, 430d)
  val defence2 = new Zone(1030d, 1090d, 430d, 490d)
  val defence3 = new Zone(1165d, 1125d, 490d, 550d)
  val globalUp = new Zone(65d, 1135d, 150d, 460d)
  val globalBottom = new Zone(65d, 1135d, 461d, 770d)
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

class Zone(val left: Double, val right : Double, val top:Double, val bottom:Double) extends Rectangle {
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
}

class MyStrategy extends Strategy {
  import MyStrategy._
  def move(self: Hockeyist, world: World, game: Game, move: Move) = {
    println(activeHockeyists(world)(teammate).map(_.teammateIndex))
    val zones= hockeyistZones.get(self.id).toList.flatten
    val currentZone = detectZone(self)
    val updatedZones = zones match {
      case p::pp::Nil if p != currentZone && pp != currentZone => currentZone :: p :: Nil
      case p :: pp :: Nil => zones
      case _ => currentZone :: zones
    }
    hockeyistZones.updated(self.id, updatedZones)
    zones.contains(upperZone)

    if (self.teammateIndex == 0) {
      if(globalUp.contains(world.puck))
        moveBottom(self, move)
      else moveUp(self, move)
    } else {

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
            moveToPuck(self, world.puck, move)

            //strikeNearestOpponent(self, world, game, move)
          }
      }
    }
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

  def moveBottom(self: Hockeyist, move: Move) = {
    if (defence1.contains(self)) {
      move.speedUp = 1.0D
      move.turn = self.angleTo(defence2.center._1, defence2.center._2)
    } else if (defence2.contains(self)) {
      move.speedUp = 0.3D
      move.turn = self.angleTo(defence3.center._1, defence3.center._2)
    } else if (defence3.contains(self)) {
      move.speedUp = 0.0D
      move.turn = self.angleTo(defence3.center._1, defence3.center._2)
    }

  }
  def moveUp(self: Hockeyist, move: Move) = {
    if (defence3.contains(self)) {
      move.speedUp = 1.0D
      move.turn = self.angleTo(defence2.center._1, defence2.center._2)
    } else if (defence2.contains(self)) {
      move.speedUp = 0.3D
      move.turn = self.angleTo(defence1.center._1, defence1.center._2)
    } else if (defence1.contains(self)) {
      move.speedUp = 0.0D
      move.turn = self.angleTo(defence1.center._1, defence1.center._2)
    }
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



  private def drivePuckToHook(self: Hockeyist, world: World, game: Game, move: Move) {
    val net = new Net(world.opponentPlayer.get, world)
    val angle = self.angleTo(world.puck)
    val nearbyenemies = opponentsInRadius(self.x, self.y, world, 80)

    if (upperZone.containsCenter(self)) {
      val angleToNet = self.angleTo(50, 560)
      move.turn = angleToNet
      if (math.abs(angleToNet) < StrikeAngle) {
        move.action = ActionType.Swing
      }
    }/* else if (targetLowerZone.containsCenter(self)) {
      val angleToNet = self.angleTo(50, 360)
      move.turn = angleToNet
      if (math.abs(angleToNet) < StrikeAngle) {
        move.action = ActionType.Swing
      }
    }*/ else if (uZone2.containsCenter(self)){
      moveToSubzone(self, move, uZone1)
    } else if (uZone1.containsCenter(self)){
      moveToSubzone(self, move, upperZone)
    }/*else if (upperZone.contains(self)) {

      move.turn = self.angleTo(targetZone.center._1, targetZone.center._2)
      move.action = ActionType.None
      move.speedUp = 1.0
    } */else {
      //moveToSubzone(self, move, nearest(self, targetLowerZone::targetUpperZone::Nil))
      moveToSubzone(self, move, uZone2)
    }
  }

  def moveToSubzone(self: Hockeyist, move: Move, zone:Zone) = {
    moveTo(self, zone.center._1, zone.center._2, move)
  }
 /* private def drivePuckToHook2(self: Hockeyist, world: World, game: Game, move: Move) {
    val net = new Net(world.opponentPlayer.get, world)
    if (net.goalieAtMargin.getOrElse(true)) {
      val angleToNet = self.angleTo(net.strikeableX, net.unguardedSide.getOrElse(net.top))
      move.turn = angleToNet
      if (math.abs(angleToNet) < StrikeAngle) {
        move.action = ActionType.Swing
      }
    } else {
      val angleToGuardedSide = self.angleTo(net.goalieLine + world.puck.radius*2, net.guardedSide.getOrElse(net.middleY))
      move.turn = angleToGuardedSide
      move.action = ActionType.None
      move.speedUp = 1.0
      //move to margin
    }
  }*/

  class Net(player:Player, world: World) extends Rectangle {
    def top:Double = player.netTop
    def bottom:Double = player.netBottom
    def left:Double = player.netLeft
    def right:Double = player.netRight

    val goalie:Option[Hockeyist] = playerGoalie(player, world)

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
