package lt.vpranckaitis.yamlg.exploration

import lt.vpranckaitis.yamlg.game.Board
import scala.collection._

case class AlphaBetaMemoizedPruning() extends Exploration {
  
  val memory = mutable.HashMap[(Board, Boolean), (Score, Moves, Board)]()

  @inline override def canRecall(b: Board, maximize: Boolean): Boolean = memory.contains((b, maximize))
  @inline override def recall(b: Board, maximize: Boolean): (Score, Moves, Board) = memory((b, maximize))
  @inline override def memoize(b: Board, maximize: Boolean, score: Score, moves: Moves, target: Board): Unit = 
    memory.put((b, maximize), (score, moves, target))
  
  @inline override def shouldPrune(alpha: Score, beta: Score): Boolean = alpha >= beta
}
