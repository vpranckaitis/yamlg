package lt.vpranckaitis.yamlg.game

import scala.annotation.tailrec
import scala.collection.{Seq, Set}
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.util.hashing.MurmurHash3
import lt.vpranckaitis.math.Math.{DistanceFunction, manhattanDistance}
import lt.vpranckaitis.yamlg.learning.NeuralNetwork

object Board {
  val height = 8
  val width = 8
  val piecesHeight = 3
  val piecesWidth = 4
  val pieces = piecesHeight * piecesWidth
  
  val emptySquare = '0'
  val cpuSquare = '1'
  val playerSquare = '2'
  
  val moves = Seq((1, 0), (-1, 0), (0, 1), (0, -1))
  val movesForward = Seq((1, 0), (0, 1))
  val winSet = {
    for {
      x <- 4 to 7
      y <- 5 to 7
    } yield (x, y)
  }.toSet
  
  val loseSet = {
    for {
      x <- 0 to 3
      y <- 0 to 2
    } yield (x, y)
  }.toSet
  
  val defaultScoring = new RandomizedTargetRectScore
  
  def distanceFunction: DistanceFunction = manhattanDistance
  
  def distance1(x: Int, y: Int) = distanceFunction(width, height)(x, y)
  
  val minDistance = { 
    def dist = distanceFunction(0, 0)
    val everyDistance = for {
      y <- 1 to piecesHeight; 
      x <- 1 to piecesWidth
    } yield dist(x, y)
    
    everyDistance.reduce(_ + _)
  }
  
  def apply(arrangement: String, scoring: Score = defaultScoring): Board = {
    apply(arrangement, '1', true, scoring)
  }
  
  def apply(arrangement: String, side: Char, dir: Boolean, scoring: Score): Board = {
    val grouped = (arrangement.zipWithIndex groupBy { _._1 }).withDefaultValue(Seq())
    
    val own = HashSet((grouped('1') map { t => posToCoord(t._2) }): _*)
    val other = HashSet(grouped('2') map { t => posToCoord(t._2) }: _*)
    new Board(own, other, (0, 0, 0, 0), null, scoring)
  }
  
  def posToCoord(p: Int) = (p % width, p / width)
  def coordToPos(x: Int, y: Int) = y * width + x
  
  def isValid(arrangement: String) = {
    (arrangement.length == height * width) && 
      List(cpuSquare, playerSquare).forall(c => (arrangement.count(_ == c) == pieces))
  }
  
  def inside(x: Int, y: Int) = { x >= 0 && x < Board.width && y >= 0 && y < Board.height}
  
  object Algorithms {
    def stringHash(b: Board) = { //slow
      b.toString.hashCode
    }
    
    def combinedSetsHash(b: Board) = { //faster
      MurmurHash3.setHash(b.own | (b.other map { xy => (xy._1 + width, xy._2 + height) })) 
    }
  }
}

case class Board(val own: HashSet[(Int, Int)], val other: HashSet[(Int, Int)], val before: (Int, Int, Int, Int), val parent: Board, val scoring: Score) {
  import lt.vpranckaitis.yamlg.game.Board._
  
  def add(xy1: (Int, Int), xy2: (Int, Int)) = (xy1._1 + xy2._1, xy1._2 + xy2._1)
  
  lazy val ownList = own.toList
  
  lazy val positionSum = ownList reduce { add(_, _) }
  
  lazy val distance = {
    def dist = distanceFunction(Board.width, Board.height)
    ownList map { xy => dist(xy._1, xy._2) } reduce { _ + _ }
  }
  
  lazy val diagonalDistance = {
    ownList map { xy => Math.max(xy._1, xy._2) - Math.min(xy._1, xy._2) } reduce { _ + _ }
  }
  
  lazy val vecticalDistance = {
    ownList map { Board.height - _._2 } reduce { _ + _ }
  }
  
  lazy val scatter = {
    def dist = distanceFunction(centre._1, centre._2)
    ownList map { xy => dist(xy._1, xy._2) } reduce { _ + _ }
  }
  
  lazy val centre = {
    val sum = ownList reduce { add(_,  _) } 
    (positionSum._1/ pieces, positionSum._2/ pieces)
  }
  
  lazy val isLeaf: Int = if (own == Board.winSet) 1 else if (other == Board.loseSet) -1 else 0
  
  lazy val score = /*NeuralNetwork.score(this)*/ scoring.evaluate(this)
  
  def move(x1: Int, y1: Int)(x2: Int, y2: Int) = {
    if (own.contains((x1, y1))) {
      new Board(own - Tuple2(x1, y1) + Tuple2(x2, y2), other, (x1, y1, x2, y2), this, scoring)
    } else {
      new Board(own, other - Tuple2(x1, y1) + Tuple2(x2, y2), (x1, y1, x2, y2), this, scoring)
    }
  }
  
  lazy val reverse = {
    def mirror(x: Int, y: Int) = (width - x - 1, height - y - 1)
    val newOwn = other map { xy => mirror(xy._1, xy._2) }
    val newOther = own map { xy => mirror(xy._1, xy._2) }
    new Board(newOwn, newOther, (0, 0, 0, 0), null, scoring)
  }
  
  def getChildren(cpuTurn: Boolean = true) = {
    val activeSide = if (cpuTurn) own else other
    val jumping = activeSide flatMap { xy => jump(xy._1, xy._2) }
    val stepping = activeSide flatMap { xy => step(xy._1, xy._2) }
    jumping ++ stepping
  }
  
  def jump(x0: Int, y0: Int): Seq[Board] = {
    def canJump(x: Int, y: Int, dx: Int, dy: Int) = (own.contains((x + dx, y + dy)) || other.contains((x + dx, y + dy))) &&
        (!own.contains((x + dx * 2, y + dy * 2)) && !other.contains((x + dx * 2, y + dy * 2))) &&
        inside(x + dx * 2, y + dy * 2)
    
    @tailrec
    def jumpRec(queue: List[Int], visited: mutable.BitSet, acc: Seq[Int]): Seq[Board] = {
      if (queue.isEmpty) {
        acc map { pos => 
          val (x1, y1) = posToCoord(pos)
          this.move(x0, y0)(x1, y1) 
        }
      } else {
        val (x1 ,y1) = posToCoord(queue.head)
        
        val succ = for { 
          xy <- moves
          dx = xy._1
          dy = xy._2
          pos = coordToPos(x1 + dx * 2, y1 + dy * 2)
          if canJump(x1, y1, dx, dy) &&
          !visited.contains(pos)
        } yield pos
        
        if (succ.isEmpty) {
          val acc1 = if (queue.head != coordToPos(x0, y0)) {
            queue.head +: acc
          } else {
            acc
          }
          jumpRec(queue.tail, visited, acc1)
        } else {
          val queue1 = queue.tail ++ succ
          visited ++= succ
          jumpRec(queue1, visited, acc)
        }
      }
    }
    
    val pos = coordToPos(x0, y0)
    val visited = mutable.BitSet(pos)
    jumpRec(List(pos), visited, Seq()) filterNot { _ == this }
  }
  
  def step(x: Int, y: Int): Seq[Board] = {
    def canStep(dx: Int, dy: Int) = inside(x + dx, y + dy) && 
      !own.contains((x + dx, y + dy)) && 
      !other.contains((x + dx, y + dy))
      
    for { 
      xy <- moves 
      if canStep(xy._1, xy._2)
    } yield this.move(x, y)(x + xy._1, y + xy._2)
  }
  
  override def toString() = {
    val chars = for {
      y <- 0 until Board.width
      x <- 0 until Board.height
    } yield if (own(x, y)) '1' else if (other(x, y)) '2' else '0'
    val strBuild = chars.foldLeft(new StringBuilder(Board.width * Board.height * 2)) {
      (acc: StringBuilder, b: Char) => acc.append(b)
    }
    "\n" ++ strBuild.toString().grouped(Board.width).mkString("\n")
  }
  
  override def equals(that: Any) = {
    if (that.getClass() == this.getClass) {
      val t = that.asInstanceOf[Board]
      (own.equals(t.own)) && (other.equals(t.other))
    } else { 
      false
    }
  }
  
  override lazy val hashCode = {
    Algorithms.combinedSetsHash(this)
  }
}