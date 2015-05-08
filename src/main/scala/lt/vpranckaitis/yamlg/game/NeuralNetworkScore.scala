package lt.vpranckaitis.yamlg.game

import lt.vpranckaitis.yamlg.learning.NeuralNetwork

class NeuralNetworkScore extends EvaluationFunction {
  override def evaluate(b: Board): Double = NeuralNetwork.evaluate(b)
}