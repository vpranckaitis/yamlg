package lt.vpranckaitis.yamlg.game

trait Score {
  def evaluate(b: Board): Double = 0
}