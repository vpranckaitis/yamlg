package lt.vpranckaitis.yamlg.learning

import lt.vpranckaitis.yamlg.game.Board
import breeze.linalg.DenseVector
import scala.collection.immutable.HashSet
import breeze.linalg.DenseMatrix
import breeze.stats.distributions.Rand
import breeze.numerics.sigmoid

object NeuralNetwork {
  val inputSize = Board.width * Board.height * 2
  val layer1Size = 40
  val outputSize = 1
  
  val theta1 = DenseMatrix.rand[Double](layer1Size, inputSize, Rand.uniform) * 0.1
  val theta2 = DenseMatrix.rand[Double](outputSize, layer1Size, Rand.uniform) * 0.1
  
  def boardToVector(b: Board): DenseVector[Double] = {
    def mapping(s: HashSet[(Int, Int)])(p: Int) = if (s.contains(Board.posToCoord(p))) 1.0 else 0.0
    
    val range = (0 until (Board.width * Board.height))
    val arrayOwn = range map { mapping(b.own) }
    val arrayOther = range map { mapping(b.other) }
    DenseVector((arrayOwn ++ arrayOther): _*)
  }
  
  def score(b: Board): Double = {
    sigmoid(theta2 * sigmoid(theta1 * boardToVector(b))).apply(0)
  }
  
}