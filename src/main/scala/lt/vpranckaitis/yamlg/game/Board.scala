package lt.vpranckaitis.yamlg.game

import scala.annotation.tailrec
import scala.collection.{Seq, Set}
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.util.hashing.MurmurHash3

import lt.vpranckaitis.math.Math.{DistanceFunction, manhattanDistance}

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
  
  def apply(arrangement: String): Board = {
    apply(arrangement, '1', true)
  }
  
  def apply(arrangement: String, side: Char, dir: Boolean): Board = {
    val grouped = arrangement.zipWithIndex groupBy { _._1 }
    
    def posToCoord(a: Int) = (a % width, a / width)
    
    val own = HashSet((grouped('1') map { t => posToCoord(t._2) }): _*)
    val other = HashSet(grouped('2') map { t => posToCoord(t._2) }: _*)
    new Board(own, other, (0, 0, 0, 0), null)
  }
  
  def isValid(arrangement: String) = {
    (arrangement.length == height * width) && 
      List(cpuSquare, playerSquare).forall(c => (arrangement.count(_ == c) == pieces))
  }
  
  object Algorithms {
    def stringHash(b: Board) = { //slow
      b.toString.hashCode
    }
    
    def combinedSetsHash(b: Board) = { //faster
      MurmurHash3.setHash(b.own | (b.other map { xy => (xy._1 + width, xy._2 + height) })) 
    }
  }
}

case class Board(val own: HashSet[(Int, Int)], val other: HashSet[(Int, Int)], val before: (Int, Int, Int, Int), val parent: Board) {
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
  
  def move(x1: Int, y1: Int)(x2: Int, y2: Int) = {
    new Board(own - Tuple2(x1, y1) + Tuple2(x2, y2), other, (x1, y1, x2, y2), this)
  }
  
  def step(p: Int, maxJumps: Int, filterDistance: Double) = {
    def inside(x: Int, y: Int) = { x >= 0 && x < Board.width && y >= 0 && y < Board.height} 
    
    def neighbouringBoards(x: Int, y: Int) = {
      
      val moves = List((1, 0), (0, 1), (-1, 0), (0, -1))
      
      def moveFromCurrent: (Int, Int) => Board = move(x, y)
      
      val stepAway = for {
        (dx, dy) <- moves
        if inside(x + dx, y + dy) && !own(x + dx, y + dy) && !other(x + dx, y + dy) 
      } yield moveFromCurrent(x + dx, y + dy)
      stepAway
    }
    
    val cascadedBoards = for {
      (x, y) <- own
    } yield neighbouringBoards(x, y)
    
    cascadedBoards.flatten
  }
  
  def dfs(nn: Int): List[Board] = {
    def childrenNotVisited(parent: Board, visited: List[Board]): Set[Board] =
      parent.step(1, 5, 10000000) filter (x => !visited.contains(x))

    @annotation.tailrec
    def loop(stack: Set[(Board, Int)], visited: List[Board]): List[Board] = {
      if (stack.isEmpty) visited
      else loop((if (stack.head._2 < nn) childrenNotVisited(stack.head._1, visited).map((_, stack.head._2 + 1)) else Set()) ++ stack.tail, 
        stack.head._1 :: visited)
    }
    loop(Set((this, 0)), Nil) reverse
  }
  
  def dfss(maxDepth: Int, visited: mutable.HashSet[Board], queue: mutable.PriorityQueue[Double]): (Board, Double) = {
    
    visited += this
    
    if (maxDepth == 0) {
      (this, distance + diagonalDistance * 0.5 + scatter *0.5 + vecticalDistance * 0.78)
    } else {
      def inside(x: Int, y: Int) = { x >= 0 && x < Board.width && y >= 0 && y < Board.height}
      def nextMoves(x: Int, y: Int) = {
        moves filter { xy => 
          inside(x + xy._1, y + xy._2) && 
          !own.contains((x + xy._1, y + xy._2)) && 
          !other.contains((x + xy._1, y + xy._2))
        } map { xy => this.move(x, y)(x + xy._1, y + xy._2) } filterNot { x => visited.contains(x) }
      }
      val bs = (for (xy <- own.toSeq) yield { 
        nextMoves(xy._1, xy._2) map { _.dfss(maxDepth - 1, visited, queue) } 
      }).flatten
      //println(bs)
        val best = bs minBy { _._2 }
        (best._1, best._2)
      
       
    }
  }
  
  override def toString() = {
    val chars = for {
      y <- 0 until Board.width
      x <- 0 until Board.height
    } yield if (own(x, y)) '1' else if (other(x, y)) '2' else '0'
    val strBuild = chars.foldLeft(new StringBuilder(Board.width * Board.height)) {
      (acc: StringBuilder, b: Char) => acc.append(b)
    }
    strBuild.toString().grouped(8) mkString "\n"
  }
  
  override lazy val hashCode = {
    Algorithms.combinedSetsHash(this)
  }
}