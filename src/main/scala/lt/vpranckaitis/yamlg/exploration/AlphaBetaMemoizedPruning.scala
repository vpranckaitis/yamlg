package lt.vpranckaitis.yamlg.exploration

import lt.vpranckaitis.yamlg.game.Board
import scala.collection._

case class AlphaBetaMemoizedPruning() extends Exploration {
  
  val memory = mutable.HashMap[(Board, Boolean), (Score, Board)]()
  
  override def canRecall(b: Board, maximize: Boolean): Boolean = memory.contains((b, maximize))
  override def recall(b: Board, maximize: Boolean): (Score, Board) = memory((b, maximize))
  override def memoize(b: Board, maximize: Boolean, score: Score, target: Board): Unit = 
    memory.put((b, maximize), (score, target))
  
  override def shouldPrune(alpha: Score, beta: Score): Boolean = alpha >= beta
}