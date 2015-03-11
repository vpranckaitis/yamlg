package lt.vpranckaitis.yamlg

import spray.routing.HttpServiceActor
import lt.vpranckaitis.yamlg.exploration.AlphaBetaMemoizedPruning
import lt.vpranckaitis.yamlg.game.Board

class WebApiActor extends HttpServiceActor {
  def receive = runRoute {
    path("move" / Segment) { b =>
      get {
        val b1 = Board(b)
        val t1 = AlphaBetaMemoizedPruning().explore(b1, 3)
        def bubbleUp(b: Board, t: Board): Board = if (b != t && b.parent != null && b.parent != t) bubbleUp(b.parent, t) else b 
        complete(bubbleUp(t1._2, b1).toString filter { Set('0', '1', '2').contains }) 
      }
    }
  }
}