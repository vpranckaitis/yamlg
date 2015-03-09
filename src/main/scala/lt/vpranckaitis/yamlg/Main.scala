package lt.vpranckaitis.yamlg

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import spray.can.Http
import lt.vpranckaitis.yamlg.game.Board
import scala.collection.immutable.TreeSet
import scala.collection.immutable.BitSet
import scala.collection.immutable.HashSet
import lt.vpranckaitis.Benchmark
import scala.collection._
import scala.util.hashing.MurmurHash3
import scala.annotation.tailrec
import lt.vpranckaitis.math.Math
import lt.vpranckaitis.yamlg.exploration.Minimax
import lt.vpranckaitis.yamlg.exploration.AlphaBetaPruning
import lt.vpranckaitis.yamlg.exploration.AlphaBetaMemoizedPruning
import lt.vpranckaitis.yamlg.learning.NeuralNetwork

object Main {
  //implicit val system = ActorSystem("yamlg")
  
  def main(args: Array[String]): Unit = {
    //val webApiActor = system.actorOf(Props[WebApiActor])
    
    //IO(Http) ! Http.Bind.apply(webApiActor, interface = "0.0.0.0", port = 8080)
    
    //Thread.sleep(3000)
    
    //Benchmark.benchmarkSets
    //Benchmark.benchmarkLoops
    
    val board = Board("1111000011110000111100000000000000000000000022220000222200002222")
    
    val input = NeuralNetwork.boardToVector(board)
    
    println(NeuralNetwork.score(board))
    
    val alphaBeta = AlphaBetaMemoizedPruning()
    
    def bubbleUp(b: Board, t: Board): Board = if (b != t && b.parent != null && b.parent != t) bubbleUp(b.parent, t) else b 
    
    def rec (b: Board) {
      val (_, bestBoard) = alphaBeta.explore(b, 3)
      val t = bubbleUp(bestBoard, b)
      println(t)
      val enemy = t.reverse
      val (_, bestBoard1) = alphaBeta.explore(enemy, 3)
      val t1 = bubbleUp(bestBoard1, enemy)
      println(t1.reverse)
      
      //Thread.sleep(250)
      rec(t1.reverse)
    }
    
    rec(board)
    
    //println(v5.size)
  }
}