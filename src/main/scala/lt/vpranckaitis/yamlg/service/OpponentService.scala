package lt.vpranckaitis.yamlg.service

import lt.vpranckaitis.yamlg.dto
import scala.concurrent.Future
import lt.vpranckaitis.yamlg.exploration.AlphaBetaMemoizedPruning
import lt.vpranckaitis.yamlg.game.Board
import scala.concurrent.ExecutionContext.Implicits.global
import lt.vpranckaitis.yamlg.learning.NeuralNetwork
import scala.annotation.tailrec
import lt.vpranckaitis.yamlg.exploration.AlphaBetaPruning

class OpponentService {
  
  def move(b: dto.Board): dto.Board = {
    val b1 = Board(b.board)
    val t1 = AlphaBetaPruning().explore(b1, 3)
    def bubbleUp(b: Board, t: Board): Board = if (b != t && b.parent != null && b.parent != t) {
      bubbleUp(b.parent, t) 
    } else {
      b
    }
    val nextBoard = bubbleUp(t1._3, b1)
    dto.Board(nextBoard.toString filter { Set('0', '1', '2').contains })
  }
  
  def reinforcedLearning(game: dto.Game) {
    @tailrec
    def separateTR[T](a: Seq[T], k: Int = 0, odd: Seq[T] = Seq(), even: Seq[T] = Seq()): (Seq[T], Seq[T]) = {
      if (a.isEmpty) {
        (odd.reverse, even.reverse) 
      } else {
        val (newOdd, newEven) = if (k == 0) {
          (a.head +: odd, even)
        } else {
          (odd, a.head +: even)
        }
        separateTR(a.tail, (k + 1) % 2, newOdd, newEven)
      }
    }
    
    def reverse(moves: Seq[String]) = {
      val reverseMapping = Map('1' -> '2', '2' -> '1', '0' -> '0')
      moves map { _.reverse map reverseMapping } 
    }
    
    val (odd, even) = { 
      val t = separateTR(game.boards)
      (t._1 map { _.board }, t._2 map { _.board })
    }
    val (winner, loser) = if (game.started == 1) {
      if (game.winner == 1) {
        (odd, reverse(even))
      } else {
        (reverse(even), odd)
      }
    } else {
      if (game.winner == 1) {
        (even, reverse(odd))
      } else {
        (reverse(odd), even)
      }
    }
    NeuralNetwork.learnReinforced(winner, 1)
    NeuralNetwork.learnReinforced(loser, -1) 
    println("learned game: " + game.gameId)
  }
}