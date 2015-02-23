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
  
  val minDistance = { 
    def dist = distanceFunction(0, 0)
    val everyDistance = for {
      i <- 1 to piecesHeight; 
      j <- 1 to piecesWidth
    } yield dist(i, j)
    
    everyDistance.reduce(_ + _)
  }
  
  def apply(arrangement: String, side: Char, dir: Boolean) = new Board(arrangement, side, true)
  def isValid(arrangement: String) = {
    (arrangement.length == height * width) && 
      List(cpuSquare, playerSquare).forall(c => (arrangement.count(_ == c) == pieces))
  }
}

class Board(arrangement: String, side: Char, dir: Boolean) {
  import lt.vpranckaitis.yamlg.game.Board._
  
  private val board: Array[Array[Int]] = {
    val mapping: Map[Char, Int] = Map(emptySquare -> 0, cpuSquare -> 1, playerSquare -> 2)
    
    def deserialize(s: String) = s map { mapping } toArray
    
    arrangement.grouped(Board.width) map { deserialize } toArray
  }
  
  lazy val distance = {
    def dist = distanceFunction(Board.width, Board.height)
    
    val everyDistance = for {
      i <- 0 until height; 
      j <- 0 until width 
      if board(i)(j) == 1
    } yield dist(j, i)
                                  
    everyDistance.reduce(_ + _)
  }
}