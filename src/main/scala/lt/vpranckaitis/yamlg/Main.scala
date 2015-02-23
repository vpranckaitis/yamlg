package lt.vpranckaitis.yamlg

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import spray.can.Http
import lt.vpranckaitis.yamlg.game.Board

object Main {
  implicit val system = ActorSystem("yamlg")
  
  def main(args: Array[String]): Unit = {
    val webApiActor = system.actorOf(Props[WebApiActor])
    
    IO(Http) ! Http.Bind.apply(webApiActor, interface = "0.0.0.0", port = 8080)
  }
}