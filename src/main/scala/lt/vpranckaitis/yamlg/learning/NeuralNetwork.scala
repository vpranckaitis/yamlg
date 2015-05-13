package lt.vpranckaitis.yamlg.learning

import java.lang.Math.log
import scala.collection.immutable.HashSet
import breeze.linalg.{DenseMatrix, DenseVector, InjectNumericOps}
import breeze.numerics.sigmoid
import breeze.optimize.{DiffFunction, LBFGS}
import breeze.stats.distributions.Rand
import lt.vpranckaitis.yamlg.game.Board
import scala.annotation.tailrec
import java.io.ObjectInputStream
import java.io.FileInputStream
import java.io.File
import java.io.ObjectOutputStream
import java.io.FileOutputStream

object NeuralNetwork {
  val inputSize = Board.width * Board.height * 2
  val layer1Size = 40
  val outputSize = 1
  
  val matrix1FileName = "m1.obj"
  val matrix2FileName = "m2.obj"
  
  val theta1 = {
    val file = new File(matrix1FileName)
    if (file.exists()) {
      println("loaded serialized matrix")
      new ObjectInputStream(new FileInputStream(file)).readObject().asInstanceOf[DenseMatrix[Double]]
    } else {
      DenseMatrix.rand[Double](layer1Size, inputSize + 1, Rand.uniform) map { x => 2*x - 1}
    }
  }
  val theta2 = {
    val file = new File(matrix2FileName)
    if (file.exists()) {
      println("loaded serialized matrix")
      new ObjectInputStream(new FileInputStream(file)).readObject().asInstanceOf[DenseMatrix[Double]]
    } else {
      DenseMatrix.rand[Double](outputSize, layer1Size + 1, Rand.uniform) map { x => 2*x - 1}
    }
  }
  
  val alpha = 0.05
  val lambda = 1.5
  
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
  
  def learnSupervised(xy: List[(DenseVector[Double], Double)]) {
    val f = new DiffFunction[DenseVector[Double]] {
      override def calculate(l: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val t1 = l.slice(0, (inputSize + 1) * layer1Size).toDenseMatrix.reshape(layer1Size, inputSize + 1)
        val t2 = l.slice((inputSize + 1) * layer1Size, l.length).toDenseMatrix.reshape(outputSize, layer1Size + 1)
        
        var J = 0.0
        val t1Grad = t1 * 0.0
        val t2Grad = t2 * 0.0
        
        for (xy <- xy) {
          //feed forward
          val x = DenseVector.vertcat(DenseVector(1.0), xy._1) 
          val y = xy._2
          val a1 = x
          val z2 = t1 * a1
          val a2 = DenseVector.vertcat(DenseVector(1.0), sigmoid(z2))
          val a3 = sigmoid(t2 * a2)
          J += -y * log(a3(0)) - (1 - y) * log(1 - a3(0))
          
          //backpropagation
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
    //println(f.valueAt(tt))
    theta1 := tt.slice(0, (inputSize + 1) * layer1Size).toDenseMatrix.reshape(layer1Size, inputSize + 1)
    theta2 := tt.slice((inputSize + 1) * layer1Size, tt.length).toDenseMatrix.reshape(outputSize, layer1Size + 1)
  }
  
  def learnReinforced(moves: Seq[String], award: Int) {
    val t1 = theta1.copy
    val t2 = theta2.copy
    
    val e1 = DenseMatrix.zeros[Double](t1.rows, t1.cols)
    val e2 = DenseMatrix.zeros[Double](t2.rows, t2.cols)
    
    def evaluate(x: DenseVector[Double]): Double = {
      val a1 = x
      val z2 = t1 * a1
      val a2 = DenseVector.vertcat(DenseVector(1.0), sigmoid(z2))
      val a3 = sigmoid(t2 * a2)
      a3(0)
    }
    
    def boardToArray(board: String) = {
      (board map { c => if (c == '1') 1.0 else 0.0 }) ++ 
      (board map { c => if (c == '2') 1.0 else 0.0 })
    }
    
    @tailrec
    def learnTR(moves: Seq[String]) {
      if (!moves.tail.isEmpty) {
        
        val s = moves.head
        val s1 = moves.tail.head
        
        val r = moves match {
          case _ :: Nil => award
          case _ => 0
        }
        
        //feed forward
        val x = DenseVector(1.0 +: boardToArray(s): _*)
        val a1 = x
        val z2 = t1 * a1
        val a2 = DenseVector.vertcat(DenseVector(1.0), sigmoid(z2))
        val a3 = sigmoid(t2 * a2)
        val v = a3(0)
        
        val v1 = evaluate(DenseVector(1.0 +: boardToArray(s1): _*))
        
        //backpropagation
        val d3 = DenseVector[Double](Math.min(1, v + 0.1))
        val d2t = (t2.t * d3) :* DenseVector.vertcat(DenseVector(1.0), sigmoidGradient(z2))
        val d2 = d2t(1 to -1)
        val t1Grad = d2 * a1.t
        val t2Grad = d3 * a2.t
        
        e1 := lambda * e1 + t1Grad
        e2 := lambda * e2 + t2Grad
        
        t1 := t1 + alpha * (r + v1 - v) * e1
        t2 := t2 + alpha * (r + v1 - v) * e2
        
        learnTR(moves.tail)
      }
    }
    
    learnTR(moves)
    
    theta1 := t1
    theta2 := t2
    
    serializeMatrix(theta1, matrix1FileName)
    serializeMatrix(theta2, matrix2FileName)
  }
  
  private def serializeMatrix(m: Any, fileName: String) {
    new ObjectOutputStream(new FileOutputStream(new File(fileName))).writeObject(m)
  }
  
  def evaluate(b: Board) = {
    val biasedInput = DenseVector.vertcat(DenseVector(1.0), boardToVector(b))
    val a2 = DenseVector.vertcat(DenseVector(1.0), sigmoid(theta1 * biasedInput))
    sigmoid(theta2 * a2).apply(0)
  }
  
}