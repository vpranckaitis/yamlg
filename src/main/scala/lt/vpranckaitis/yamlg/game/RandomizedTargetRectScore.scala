package lt.vpranckaitis.yamlg.game

class RandomizedTargetRectScore(val range: Double = 0.3) extends TargetRectScore {
  
  override def evaluate(b: Board) = super.evaluate(b) * (Math.random() * 2  * range + (1 - range))
}