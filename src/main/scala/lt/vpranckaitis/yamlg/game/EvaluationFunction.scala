package lt.vpranckaitis.yamlg.game

trait EvaluationFunction {
  def evaluate(b: Board): Double = 0
}

object EvaluationFunction {
  private val mapping = Map(0 -> new TargetRectScore, 
                            1 -> new RandomizedTargetRectScore(0.1),
                            2 -> new RandomizedTargetRectScore(0.3),
                            3 -> new NeuralNetworkScore) 
  
  def apply(k: Int): EvaluationFunction = {
    mapping.getOrElse(k, new TargetRectScore)
  }
}