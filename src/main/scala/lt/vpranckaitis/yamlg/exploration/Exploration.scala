package lt.vpranckaitis.yamlg.exploration

import scala.annotation.tailrec
import scala.collection.Seq

import lt.vpranckaitis.yamlg.game.Board

/**
 * Template method pattern for minimax with alpha-beta pruning
 */
trait Exploration {
  
  type Score = Double
  type Moves = Int
  
  def canRecall(b: Board, maximize: Boolean): Boolean = false
  def recall(b: Board, maximize: Boolean): (Score, Moves, Board) = (0.0, 0, null)
  def memoize(b: Board, maximize: Boolean, score: Score, moves: Moves, target: Board): Unit = Unit
  
  def shouldPrune(alpha: Score, beta: Score): Boolean = false
  
  def explore(board: Board, maxDepth: Int, maximize: Boolean = true): (Score, Moves, Board) = {
    
    def maxValue(seq: Seq[Board], depth: Int, alpha: Score, beta: Score): (Score, Moves, Board) = {
      @tailrec
      def maxValueTR(seq: Seq[Board], s: Score, m: Moves, b: Board): (Score, Moves, Board) = {
        if (seq.isEmpty) {
          (s, m, b)
        } else if (seq.head.isLeaf == 1) {
          (Double.MaxValue/2, maxDepth - depth,  seq.head)
        } else {
          val (s1, m1, b1) = alphaBetaRec(seq.head, depth - 1, Math.max(alpha, s), beta, false)
          if (shouldPrune(s1, beta)) {
            (s1, m1, b1)
          } else {
            if ((Math.abs(s1 - s) < 1e-6 && m1 < m) || s1 > s)
              maxValueTR(seq.tail, s1, m1, b1)
            else
              maxValueTR(seq.tail, s, m, b)
          }
        }
      }
      
      maxValueTR(seq, Double.MinValue, Int.MaxValue, seq.head)
    }
    
    def minValue(seq: Seq[Board], depth: Int,  alpha: Score, beta: Score): (Score, Moves, Board) = {
      @tailrec
      def minValueTR(seq: Seq[Board], s: Score, m: Moves, b: Board): (Score, Moves, Board) = {
        if (seq.isEmpty) {
          (s, m, b)
        } else if (seq.head.isLeaf == 1) {
          (Double.MaxValue/2, maxDepth - depth,  seq.head)
        } else {
          val (s1, m1, b1) = alphaBetaRec(seq.head, depth - 1, alpha, Math.min(s, beta), true)
          if (shouldPrune(alpha, s1)) {
            (s1, m1, b1)
          } else {
            if ((Math.abs(s1 - s) < 1e-6 && m1 > m) || s1 < s)
              minValueTR(seq.tail, s1, m1, b1)
            else
              minValueTR(seq.tail, s, m, b)
          }
        }
      }
      
      minValueTR(seq, Double.MaxValue, Int.MinValue, seq.head)
    }
    
    def alphaBetaRec(b: Board, depth: Int, alpha: Score, beta: Score, maximize: Boolean): (Score, Int, Board)  = {
      if (depth == 0) {
        memoize(b, maximize, b.score, maxDepth, b)
        (b.score, maxDepth, b)
      } else {
        val children = b.getChildren(maximize).toSeq
        
        val (score, moves, leafBoard) = if (canRecall(b, maximize)) {
          recall(b, maximize)
        } else if (maximize) {
          maxValue(children, depth, alpha, beta)
        } else {
          minValue(children, depth, alpha, beta)
        }
        memoize(b, maximize, score, moves, leafBoard)
        (score, moves, leafBoard)
      }
    }
    
    alphaBetaRec(board, maxDepth, Double.MinValue, Double.MaxValue, maximize)
  }
}