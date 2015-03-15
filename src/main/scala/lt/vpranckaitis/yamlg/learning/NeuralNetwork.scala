package lt.vpranckaitis.yamlg.learning

import lt.vpranckaitis.yamlg.game.Board
import breeze.linalg.DenseVector
import scala.collection.immutable.HashSet
import breeze.linalg.DenseMatrix
import breeze.stats.distributions.Rand
import breeze.numerics.sigmoid
import breeze.optimize._
import breeze.math.NormedModule
import breeze.linalg.reshape._
import java.lang.Math._
import lt.vpranckaitis.yamlg.game.Score

object NeuralNetwork extends Score {
  val inputSize = Board.width * Board.height * 2
  val layer1Size = 40
  val outputSize = 1
  
  val theta1 = DenseMatrix.rand[Double](layer1Size, inputSize + 1, Rand.uniform) * -0.1
  val theta2 = DenseMatrix.rand[Double](outputSize, layer1Size + 1, Rand.uniform) * -0.1
  
  def boardToVector(b: Board): DenseVector[Double] = {
    def mapping(s: HashSet[(Int, Int)])(p: Int) = if (s.contains(Board.posToCoord(p))) 1.0 else 0.0
    
    val range = (0 until (Board.width * Board.height))
    val arrayOwn = range map { mapping(b.own) }
    val arrayOther = range map { mapping(b.other) }
    DenseVector((arrayOwn ++ arrayOther): _*)
  }
  
  private[this] def sigmoidGradient(m: DenseVector[Double]) = {
    val sm = sigmoid(m)
    sm :* (1.0 - sm)
  }
  
  def learn(xy: List[(DenseVector[Double], Double)]) {
    val f = new DiffFunction[DenseVector[Double]] {
      override def calculate(l: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val t1 = l.slice(0, (inputSize + 1) * layer1Size).toDenseMatrix.reshape(layer1Size, inputSize + 1)
        val t2 = l.slice((inputSize + 1) * layer1Size, l.length).toDenseMatrix.reshape(outputSize, layer1Size + 1)
        
        var J = 0.0
        val t1Grad = t1 * 0.0
        val t2Grad = t2 * 0.0
        
        for (xy <- xy) {
          val x = DenseVector.vertcat(DenseVector(1.0), xy._1) 
          val y = xy._2
          val a1 = x
          val z2 = t1 * a1
          val a2 = DenseVector.vertcat(DenseVector(1.0), sigmoid(z2))
          val a3 = sigmoid(t2 * a2)
          J += -y * log(a3(0)) - (1 - y) * log(1 - a3(0))
          
          val d3 = a3 - y
          val d2t = (t2.t * d3) :* DenseVector.vertcat(DenseVector(1.0), sigmoidGradient(z2))
          val d2 = d2t(1 to -1)
          t1Grad += d2 * a1.t
          t2Grad += d3 * a2.t
        }
        
        val m = xy.length.toDouble
        
        J /= m
        
        t1Grad :/= m
        t2Grad :/= m
        
        (J, DenseVector.vertcat(t1Grad.toDenseVector, t2Grad.toDenseVector))
      }
    }
    
    val s = new LBFGS[DenseVector[Double]](maxIter=100, m=3)
    val tt = s.minimize(f, DenseVector.vertcat(theta1.toDenseVector, theta2.toDenseVector))
    //println(tt)
    println(f.valueAt(tt))
    theta1 := tt.slice(0, (inputSize + 1) * layer1Size).toDenseMatrix.reshape(layer1Size, inputSize + 1)
    theta2 := tt.slice((inputSize + 1) * layer1Size, tt.length).toDenseMatrix.reshape(outputSize, layer1Size + 1)
  }
  
  override def evaluate(b: Board) = {
    val biasedInput = DenseVector.vertcat(DenseVector(1.0), boardToVector(b))
    val a2 = DenseVector.vertcat(DenseVector(1.0), sigmoid(theta1 * biasedInput))
    sigmoid(theta2 * a2).apply(0)
  }
  
}