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
        def bubbleUp(b: Board, t: Board): Board = if (b != t && b.parent != null && b.parent != t) {
          println(b)
          bubbleUp(b.parent, t) 
        } else {
          println(b)
          b
        }
        println("============")
        println(t1)
        println(t1._3.isLeaf)
        complete(bubbleUp(t1._3, b1).toString filter { Set('0', '1', '2').contains }) 
      }
    } ~ 
    path("n" / Segment) { b =>
      get {
        val b1 = Board(b)
        complete(b1.getChildren(true).toList sortBy { _.score } map { x => x.toString + " " + x.score } mkString "")
      }
    } ~
    path("s" / Segment) { b =>
      get {
        val b1 = Board(b)
        complete(b1.score.toString)
      }
    }
  }
}