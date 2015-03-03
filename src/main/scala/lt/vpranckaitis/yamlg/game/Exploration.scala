package lt.vpranckaitis.yamlg.game

import scala.annotation.tailrec

object Exploration {
  
  type Score = Double
  
  def alphaBeta(board: Board, depth: Int, maximize: Boolean = true): (Score, Board) = {
    
    def maxValue(seq: Seq[Board], depth: Int,  alpha: Score, beta: Score): (Score, Board) = {
      @tailrec
      def maxValueTR(seq: Seq[Board], v: Score, b: Board): (Score, Board) = {
        if (seq.isEmpty) {
          (v, b)
        } else {
          val (v1, b1) = alphaBetaRec(seq.head, depth - 1, Math.max(alpha, v), beta, false)
          if (v1 >= beta) {
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
          if (v1 <= alpha) {
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
        (b.score, b)
      } else {
        val children = b.getChildren(maximize).toSeq
        
        if (maximize) {
          maxValue(children, depth, alpha, beta)
        } else {
          minValue(children, depth, alpha, beta)
        }
      }
    }
    
    alphaBetaRec(board, depth, Double.MinValue, Double.MaxValue, maximize)
  }
}