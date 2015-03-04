package lt.vpranckaitis.yamlg.exploration

import lt.vpranckaitis.yamlg.game.Board

case class AlphaBetaPruning() extends Exploration {
  @inline override def shouldPrune(alpha: Score, beta: Score) = alpha >= beta
}
