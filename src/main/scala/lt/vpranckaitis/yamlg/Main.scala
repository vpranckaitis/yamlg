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
import com.typesafe.config.ConfigFactory
import java.io.File

object Main {
  implicit val system = ActorSystem("yamlg")
  
  def main(args: Array[String]): Unit = {
    val opponenentActor = system.actorOf(Props(classOf[OpponentServiceActor]))
    val webApiActor = system.actorOf(Props(classOf[WebApiActor], opponenentActor))
    
    val config = ConfigFactory.parseFile(new File("application.conf")).withFallback(ConfigFactory.load())
    
    val ip = config.getString("yamlg.ip")
    val port = config.getInt("yamlg.port")
    
    IO(Http) ! Http.Bind(webApiActor, interface = ip, port = port)
    
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        IO(Http) ! Http.Unbind(Duration("3s")) 
      }
    })
    
    val board = Board("1111000011110000111100000000000000000000000022220000222200002222", new TargetRectScore)
    val b1 = Board("1111000011110000111100000000000000000000000000000000000000000000", new TargetRectScore)
    val b2 = Board("0000000000000000000000000000000000000000000011110000111100001111", new TargetRectScore)
    
    NeuralNetwork.learnSupervised(List((NeuralNetwork.boardToVector(board), 0), (NeuralNetwork.boardToVector(b1), 0), (NeuralNetwork.boardToVector(b2), 1)))
    
    println(NeuralNetwork.evaluate(board))
    
  }
}