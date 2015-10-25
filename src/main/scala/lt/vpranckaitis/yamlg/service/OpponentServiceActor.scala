package lt.vpranckaitis.yamlg.service

import akka.actor.Actor
import lt.vpranckaitis.yamlg.dto
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Props
import akka.routing.Router
import akka.routing.RoundRobinPool
import com.typesafe.config.ConfigFactory
import java.io.File

object OpponentServiceActor {
  case class Move(b: dto.Board)
  case class Learn(g: dto.Game)
  case class LearnWork(g: dto.Game)
  
  val moveWorkersCount = {
    val config = ConfigFactory.parseFile(new File("application.conf")).withFallback(ConfigFactory.load())
    config.getInt("yamlg.concurrency.move-workers-count")
  }
  
  class LearnWorker(service: OpponentService) extends Actor with ActorLogging {
    def receive = {
      case LearnWork(g) => { 
        service.reinforcedLearning(g)
      }
      case _ => log.warning("unknown message")
    }
  }
  
  class MoveWorker(service: OpponentService) extends Actor with ActorLogging {
    def receive = {
      case Move(b) => sender() ! service.move(b)
    }
  }
}

class OpponentServiceActor extends Actor with ActorLogging {
  
  import OpponentServiceActor._
  
  val service = new OpponentService()
  
  val moveWorker = context.actorOf(RoundRobinPool(moveWorkersCount).props(Props(classOf[MoveWorker], service)))
  val learnWorker = context.actorOf(Props(classOf[LearnWorker], service))
  
  def receive = {
    case m: Move => moveWorker forward m
    case Learn(g) => learnWorker ! LearnWork(g)
    case _ => log.warning("unknown message")
  }
  
  
}