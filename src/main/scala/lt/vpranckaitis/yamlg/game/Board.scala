package lt.vpranckaitis.yamlg.game

import lt.vpranckaitis.math.Math._
import java.lang.Math._ 

object Board {
  val height = 8
  val width = 8
  val piecesHeight = 3
  val piecesWidth = 4
  val pieces = piecesHeight * piecesWidth
  
  val emptySquare = '0'
  val cpuSquare = '1'
  val playerSquare = '2'
  
  def distanceFunction: DistanceFunction = manhattanDistance
  
  def distance(x: Int, y: Int) = distanceFunction(width, height)(x, y)
  
  val minDistance = { 
    def dist = distanceFunction(0, 0)
    val everyDistance = for {
      y <- 1 to piecesHeight; 
      x <- 1 to piecesWidth
    } yield dist(x, y)
    
    everyDistance.reduce(_ + _)
  }
  
  def apply(arrangement: String, side: Char, dir: Boolean) = {
    val grouped = arrangement.zipWithIndex groupBy { _._1 }
    
    def posToCoord(a: Int) = (a / width, a % width)
    
    val own = (grouped('1') map { t => posToCoord(t._2) }).toSet
    val other = (grouped('2') map { t => posToCoord(t._2) }).toSet
    new Board(own, other)
  }
  
  def isValid(arrangement: String) = {
    (arrangement.length == height * width) && 
      List(cpuSquare, playerSquare).forall(c => (arrangement.count(_ == c) == pieces))
  }
}

class Board(val own: Set[(Int, Int)], val other: Set[(Int, Int)]) {
  import lt.vpranckaitis.yamlg.game.Board._
  
  lazy val distance = {
    def dist = distanceFunction(Board.width, Board.height)
    own map { xy => dist(xy._1, xy._2) } reduce { _ + _ }
  }
  
  
  
  def move(x1: Int, y1: Int)(x2: Int, y2: Int) = {
    new Board(own - Tuple2(x1, y1) + Tuple2(x2, y2), other)
  }
  
  def step(p: Int, maxJumps: Int, filterDistance: Double) = {
    def inside(x: Int, y: Int) = { x >= 0 && x < Board.width && y >= 0 && y < Board.height} 
    
    def neighbouringBoards(x: Int, y: Int) = {
      val moves = List((1, 0), (0, 1), (-1, 0), (0, -1))
      def moveFromCurrent: (Int, Int) => Board = move(x, y)
      val stepAway = for {
        (dx, dy) <- moves
        if inside(x + dx, y + dy) && !own(x + dx, y + dy) && !own(x + dx, y + dy) && filterDistance > distance - Board.distance(x, y) + Board.distance(x + dx, y + dy)
      } yield moveFromCurrent(x + dx, y + dy)
      stepAway
    }
    
    val cascadedBoards = for {
      (x, y) <- own
    } yield neighbouringBoards(x, y)
    
    cascadedBoards.flatten
  }
  
  override def toString() = {
    val chars = for {
      x <- 0 until Board.width
      y <- 0 until Board.height
    } yield if (own(x, y)) '1' else if (other(x, y)) '2' else '0'
    val strBuild = chars.foldLeft(new StringBuilder(Board.width * Board.height)) {
      (acc: StringBuilder, b: Char) => acc.append(b)
    }
    strBuild.toString()
  }
}