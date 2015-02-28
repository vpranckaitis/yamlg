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

object Main {
  //implicit val system = ActorSystem("yamlg")
  
  def main(args: Array[String]): Unit = {
    //val webApiActor = system.actorOf(Props[WebApiActor])
    
    //IO(Http) ! Http.Bind.apply(webApiActor, interface = "0.0.0.0", port = 8080)
    
    Thread.sleep(500)
    
    Benchmark.benchmarkSets
    Benchmark.benchmarkLoops
    
    /*
    val range = (1 to 1000000 by 17).toSeq
    
    val bs = BitSet(range: _*)
    val hs = HashSet[Int](range: _*)
    val ts = TreeSet(range: _*)
    
    val t1 = System.nanoTime()
    //println(bs.foldLeft(1L)(_ + _))
    for (i <- 1 to 1000000 by 19) {
      bs.contains(i)
    }
    val t2 = System.nanoTime()
    //println(hs.foldLeft(1L)(_ + _))
    for (i <- 1 to 1000000 by 19) {
      hs.contains(i)
    }
    val t3 = System.nanoTime()
    //println(ts.foldLeft(1L)(_ + _))
    for (i <- 1 to 1000000 by 19) {
      ts.contains(i)
    }
    val t4 = System.nanoTime()
    */
    
    //println("BitSet  : " + ((t2 - t1) / 1000000.0) + "\nHashSet : " + ((t3 - t2) / 1000000.0) + "\nTreeSet : " + ((t4 - t3) / 1000000.0))
    //val b = Board("1111000011110000111100000000000000000000000022220000222200002222", '1', false)
    //val b = Board("0011110001011000011110000000001000000000000022220000222200002222", '1', false)
    val b = Board("0000000010101010010101011010101000000000000022220000222200002222", '1', false)
    //val b = Board("1000000000000000000000000000000000000000000022220000222200002222", '1', false)
    val b1 = Board("0000000010101010010101011010101000000000000022220000222200002222", '1', false)
    
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
    val v2 = mutable.HashSet[String]()
    println("Func : " + Benchmark.timer { () => b.dfss(6, v1) })
   
    //println(v5.size)
  }
}