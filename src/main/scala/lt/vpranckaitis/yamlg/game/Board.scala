package lt.vpranckaitis.yamlg.game

import lt.vpranckaitis.math.Math._
import java.lang.Math._
import scala.collection.immutable.HashSet
import scala.annotation.tailrec
import akka.util.HashCode
import scala.collection._
import scala.util.hashing.MurmurHash3

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
    
    def posToCoord(a: Int) = (a % width, a / width)
    
    val own = HashSet((grouped('1') map { t => posToCoord(t._2) }): _*)
    val other = HashSet(grouped('2') map { t => posToCoord(t._2) }: _*)
    new Board(own, other)
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

case class Board(val own: HashSet[(Int, Int)], val other: HashSet[(Int, Int)]) {
  import lt.vpranckaitis.yamlg.game.Board._
  
  def add(xy1: (Int, Int), xy2: (Int, Int)) = (xy1._1 + xy2._1, xy1._2 + xy2._1)
  
  lazy val distance = {
    def dist = distanceFunction(Board.width, Board.height)
    own map { xy => dist(xy._1, xy._2) } reduce { _ + _ }
  }
  
  lazy val centre = {
    val sum = own reduce { add(_,  _) } 
    (sum._1 / pieces, sum._2 / pieces)
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
  
  

  def traverse(maxDepth: Int) = {
    @tailrec
    def traverseTR(toVisit: Seq[Board], d: Seq[Int], visited: HashSet[Board], accumulator: Seq[Board]): Seq[Board] = {
      if(toVisit.isEmpty) {
        accumulator
      } else  {
        val next = toVisit.head
        val nextDepth = d.head
        val succ = if (nextDepth < maxDepth) (next.step(1, 5, 1000000).view filterNot { visited.contains } filterNot { toVisit.contains }).toSeq else Seq.empty
        val succDepth = succ map { _ => nextDepth + 1}
        // DFS :
        traverseTR(succ ++ toVisit.tail, succDepth ++ d.tail, visited + next, next +: accumulator)
        
      }
    }
    
    traverseTR(Seq(this), Seq(0), HashSet.empty, Seq.empty).reverse
  }
  
  def dfss(maxDepth: Int, visited: mutable.HashSet[Board]): Int = {
    
    visited += this
    
    if (maxDepth == 0) {
      1
    } else {
      def inside(x: Int, y: Int) = { x >= 0 && x < Board.width && y >= 0 && y < Board.height}
      def nextMoves(x: Int, y: Int) = {
        moves.view filter { xy => 
          inside(x + xy._1, y + xy._2) && 
          !own.contains((x + xy._1, y + xy._2)) && 
          !other.contains((x + xy._1, y + xy._2))
        } map { xy => this.move(x, y)(x + xy._1, y + xy._2) } filterNot { x => visited.contains(x) }
      }
      (for (xy <- own.toSeq.view) yield { 
        (nextMoves(xy._1, xy._2).view map { _.dfss(maxDepth - 1, visited) }).foldLeft(0) { _ + _ }
      }).foldLeft(1) { _ + _ }
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