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
import akka.util.Timeout
import scala.concurrent.duration.Duration
import lt.vpranckaitis.yamlg.game.TargetRectScore
import lt.vpranckaitis.yamlg.service.OpponentServiceActor

object Main {
  implicit val system = ActorSystem("yamlg")
  
  def main(args: Array[String]): Unit = {
    val opponenentActor = system.actorOf(Props(classOf[OpponentServiceActor]))
    val webApiActor = system.actorOf(Props(classOf[WebApiActor], opponenentActor))
    
    IO(Http) ! Http.Bind(webApiActor, interface = "0.0.0.0", port = 5555)
    
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        IO(Http) ! Http.Unbind(Duration("3s")) 
      }
    })
    
    println(NeuralNetwork.theta1)
    println(NeuralNetwork.theta2)
    
    val board = Board("1111000011110000111100000000000000000000000022220000222200002222", new TargetRectScore)
    val b1 = Board("1111000011110000111100000000000000000000000000000000000000000000", new TargetRectScore)
    val b2 = Board("0000000000000000000000000000000000000000000011110000111100001111", new TargetRectScore)
    val b3 = Board("1111000011110000111100000000000000000000000000000000000000000002", new TargetRectScore)
    val b4 = Board("0000000000000000000101100000000000000111000200110000001100000011")
    val b5 = Board("2222000022220000222200000000000000000000000000000000000000000000")
    
    println(b2.isLeaf)
    println(b5.isLeaf)
    
    val input = NeuralNetwork.boardToVector(board)
    
    
    //NeuralNetwork.learnSupervised(List((NeuralNetwork.boardToVector(board), 0), (NeuralNetwork.boardToVector(b1), 0), (NeuralNetwork.boardToVector(b2), 1)))
    
    println(NeuralNetwork.evaluate(board))
    
    val alphaBeta = AlphaBetaMemoizedPruning()
    
    def bubbleUp(b: Board, t: Board): Board = if (b != t && b.parent != null && b.parent != t) bubbleUp(b.parent, t) else b 
    def printup(b: Board, t: Board): Unit = if (b != t && b.parent != null && b.parent != t) {
      println(b.toString + "**")
      bubbleUp(b.parent, t)
    }
    
    
    def rec (b: Board) {
      val (_, _, bestBoard) = alphaBeta.explore(b, 3)
      val t = bubbleUp(bestBoard, b)
      println(t.score)
      println(t)
      val enemy = t.reverse
      val (_, _, bestBoard1) = alphaBeta.explore(enemy, 3)
      val t1 = bubbleUp(bestBoard1, enemy)
      //println(bestBoard1.reverse.toString ++ "**")
      //println(bestBoard1.parent.reverse.toString ++ "**")
      //println(bestBoard1.parent.parent.reverse.toString ++ "**")
      println(t1.reverse)
      val t2 = t1.reverse
      if (t.own != t2.own && t.other != t2.other) {
        printup(bestBoard1, enemy)
      }
      //Thread.sleep(250)
      rec(t1.reverse)
    }
    
    Thread.sleep(1000)
    
    //rec(b4)
  }
}