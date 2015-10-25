package lt.vpranckaitis.yamlg

import akka.pattern.ask
import lt.vpranckaitis.yamlg.dto.ExtendedJsonProtocol._
import lt.vpranckaitis.yamlg.game.Board
import lt.vpranckaitis.yamlg.service.OpponentServiceActor
import spray.http.StatusCodes
import spray.httpx.SprayJsonSupport.{sprayJsonMarshaller, sprayJsonUnmarshaller}
import spray.routing.HttpServiceActor
import akka.util.Timeout
import scala.concurrent.duration._
import akka.actor.ActorRef
import scala.util.Success
import lt.vpranckaitis.yamlg.learning.NeuralNetwork
import lt.vpranckaitis.yamlg.game.NeuralNetworkScore

class WebApiActor(service: ActorRef) extends HttpServiceActor {
  
  import OpponentServiceActor._
  import context.dispatcher
  
  implicit val timeout = Timeout(Duration(5, SECONDS))
  
  def receive = runRoute {
    path("move" / Segment) { b =>
      get {
        onComplete(service ? Move(dto.Board(b))) { 
          case Success(result: dto.Board) => complete(result)
          case _ => complete(StatusCodes.InternalServerError, "failed")
        }
      }
    } ~ 
    path("learn" / "game") {
      put {
        entity(as[dto.Game]) { g =>
          service ! Learn(g)
          println("learning game: " + g.gameId)
          complete("{\"acknowledged\": true}")
        }
      }
    } ~
    path("n" / Segment) { b =>
      get {
        val b1 = Board(b, new NeuralNetworkScore)
        complete(b1.getChildren(true).toList sortBy { _.score } map { x => x.toString + " " + x.score } mkString "")
      }
    } ~
    path("s" / Segment) { b =>
      get {
        val b1 = Board(b)
        complete(NeuralNetwork.evaluate(b1).toString)
      }
    }
  }
}