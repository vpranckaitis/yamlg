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
import lt.vpranckaitis.yamlg.game.Exploration

object Main {
  //implicit val system = ActorSystem("yamlg")
  
  def main(args: Array[String]): Unit = {
    //val webApiActor = system.actorOf(Props[WebApiActor])
    
    //IO(Http) ! Http.Bind.apply(webApiActor, interface = "0.0.0.0", port = 8080)
    
    Thread.sleep(500)
    
    Benchmark.benchmarkSets
    Benchmark.benchmarkLoops
    
    val board = Board("1111000011110000111100000000000000000000000022220000222200002222")
    
    //println(Benchmark.timer { () => println(Exploration.explore(board, 7)) })
    
    
    //println(v5.size)
  }
}