package lt.vpranckaitis.yamlg.exploration

import scala.annotation.tailrec
import scala.collection.Seq

import lt.vpranckaitis.yamlg.game.Board

/**
 * Template method pattern for minimax with alpha-beta pruning
 */
trait Exploration {
  
  type Score = Double
  
  @inline def canRecall(b: Board, maximize: Boolean): Boolean = false
  @inline def recall(b: Board, maximize: Boolean): (Score, Board) = (0.0, null)
  @inline def memoize(b: Board, maximize: Boolean, score: Score, target: Board): Unit = Unit
  
  def shouldPrune(alpha: Score, beta: Score): Boolean = false
  
  def explore(board: Board, depth: Int, maximize: Boolean = true): (Score, Board) = {
    
    def maxValue(seq: Seq[Board], depth: Int,  alpha: Score, beta: Score): (Score, Board) = {
      @tailrec
      def maxValueTR(seq: Seq[Board], v: Score, b: Board): (Score, Board) = {
        if (seq.isEmpty) {
          (v, b)
        } else {
          val (v1, b1) = alphaBetaRec(seq.head, depth - 1, Math.max(alpha, v), beta, false)
          if (shouldPrune(v1, beta)) {
            (v1, b1)
          } else {
            maxValueTR(seq.tail, Math.max(v1, v), if (v1 > v) b1 else b)
          }
        }
      }
      
      maxValueTR(seq, Double.MinValue, board)
    }
    
    def minValue(seq: Seq[Board], depth: Int,  alpha: Score, beta: Score): (Score, Board) = {
      @tailrec
      def minValueTR(seq: Seq[Board], v: Score, b: Board): (Score, Board) = {
        if (seq.isEmpty) {
          (v, b)
        } else {
          val (v1, b1) = alphaBetaRec(seq.head, depth - 1, alpha, Math.min(v, beta), true)
          if (shouldPrune(alpha, v1)) {
            (v1, b1)
          } else {
            minValueTR(seq.tail, Math.min(v1, v), if (v1 < v) b1 else b)
          }
        }
      }
      
      minValueTR(seq, Double.MaxValue, board)
    }
    
    def alphaBetaRec(b: Board, depth: Int, alpha: Score, beta: Score, maximize: Boolean): (Score, Board)  = {
      if (depth == 0) {
        memoize(b, maximize, b.score, b)
        (b.score, b)
      } else {
        val children = b.getChildren(maximize).toSeq
        
        val (score, leafBoard) = if (canRecall(b, maximize)) {
          recall(b, maximize)
        } else if (maximize) {
          maxValue(children, depth, alpha, beta)
        } else {
          minValue(children, depth, alpha, beta)
        }
        memoize(b, maximize, score, leafBoard)
        (score, leafBoard)
      }
    }
    
    alphaBetaRec(board, depth, Double.MinValue, Double.MaxValue, maximize)
  }
}