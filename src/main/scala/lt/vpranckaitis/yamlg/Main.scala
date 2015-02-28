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

object Main {
  //implicit val system = ActorSystem("yamlg")
  
  def main(args: Array[String]): Unit = {
    //val webApiActor = system.actorOf(Props[WebApiActor])
    
    //IO(Http) ! Http.Bind.apply(webApiActor, interface = "0.0.0.0", port = 8080)
    
    Thread.sleep(500)
    
    //Benchmark.benchmarkSets
    //Benchmark.benchmarkLoops
    
    //println("BitSet  : " + ((t2 - t1) / 1000000.0) + "\nHashSet : " + ((t3 - t2) / 1000000.0) + "\nTreeSet : " + ((t4 - t3) / 1000000.0))
    //val b = Board("1111000011110000111100000000000000000000000022220000222200002222", '1', false)
    val b = Board("111100001111000011110000200000000000000000000000000000000000000", '1', false)
    //val b = Board("0011110001011000011110000000001000000000000022220000222200002222", '1', false)
    //val b = Board("0000000010101010010101011010101000000000000022220000222200002222", '1', false)
    //val b = Board("1100000000000000000000000000000000020000000000000000000000000000", '1', false)
    //val b1 = Board("0000000010101010010101011010101000000000000022220000222200002222", '1', false)
    
    //println(b.hashCode() == b1.hashCode())
    
    //println(b.toString())
    val filterDistance = b.distance + 1000000
    /*
    val v1 = b.step(1, 5, filterDistance)
    val v2 = (v1.view flatMap { _.step(1, 5, filterDistance ) }).force
    val v3 = (v2.view flatMap { _.step(1, 5, filterDistance) }).force
    val v4 = (v3.view flatMap { _.step(1, 5, filterDistance) }).force
    //val v5 = (v4.view flatMap { _.step(1, 5, filterDistance) }).force
    */
    
    //val treeset = TreeSet(9, 8, 7, 6, 5, 4, 3, 2, 1)
    //treeset.foreach { println }
    
    //val v = HashSet[String]()
    //println(b.dfss(5, v))
    //println(v mkString "\n\n")
    
    
    //println("Time: " + (System.currentTimeMillis() - t1))
    val v1 = mutable.HashSet[Board]()
    val b1 = b.dfss(1, v1, mutable.PriorityQueue[Double]())._1
    val v2 = mutable.HashSet[Board]()
    val b2 = b1.dfss(1, v1, mutable.PriorityQueue[Double]())._1
    println(b1 + "\n")
    println(b2 + "\n")
    //println(v mkString "\n\n")
    
    //println((Board("0000000100000001000000000000000000020000000000000000000000000000").own map {xy => Math.manhattanDistance(8, 8)(xy._1, xy._2)}))
    //println((Board("0000000100000010000000000000000000020000000000000000000000000000").own map {xy => Math.manhattanDistance(8, 8)(xy._1, xy._2)}))
    
    def bubble(b: Board, depth: Int): Board = if (depth == 1) b else bubble(b.parent, depth - 1) 
    
    val queue = mutable.PriorityQueue[Double]()
    val visited = mutable.HashSet[Board]()
    
    @tailrec 
    def yourTurn(current: Board, visited: mutable.HashSet[Board], queue: mutable.PriorityQueue[Double]) {
      println(current + "\n")
      if (current.distance == Board.minDistance) {
        Unit
      } else {
        val depth = 3
        queue.clear()
        visited.clear()
        val next = current.dfss(depth, visited, queue)._1
        //Thread.sleep(250)
        yourTurn(bubble(next, depth), visited, queue)
      }
    }
    
    yourTurn(b2, visited, queue)
    //println(v5.size)
  }
}