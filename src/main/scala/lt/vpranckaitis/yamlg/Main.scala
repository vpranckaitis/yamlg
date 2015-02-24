package lt.vpranckaitis.yamlg

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import spray.can.Http
import lt.vpranckaitis.yamlg.game.Board

object Main {
  implicit val system = ActorSystem("yamlg")
  
  def main(args: Array[String]): Unit = {
    val webApiActor = system.actorOf(Props[WebApiActor])
    
    //IO(Http) ! Http.Bind.apply(webApiActor, interface = "0.0.0.0", port = 8080)
    
    Thread.sleep(500)
    
    val t1 = System.currentTimeMillis()
    val b = Board("1111000011110000111100000000000000000000000022220000222200002222", '1', false)
    //val b = Board("0011110001011000011110000000001000000000000022220000222200002222", '1', false)
    
    val filterDistance = b.distance + 2
    val v1 = b.step(1, 5, filterDistance)
    println(v1.size)
    println((v1. map { _.toString.grouped(8) mkString "\n" }) mkString "\n\n")
    val v2 = v1 flatMap { _.step(1, 5, filterDistance ) } 
    println(v2.size)
    val v3 = v2 flatMap { _.step(1, 5, filterDistance) }
    println(v3.size)
    val v4 = v3 flatMap { _.step(1, 5, filterDistance) }
    println(v4.size)
    println(System.currentTimeMillis() - t1)
  }
}