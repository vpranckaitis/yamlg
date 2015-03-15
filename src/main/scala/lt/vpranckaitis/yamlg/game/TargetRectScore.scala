package lt.vpranckaitis.yamlg.game

class TargetRectScore extends EvaluationFunction {
  /*
  def toScore(xy: (Int, Int)): Int = {
    val scoreX = if (xy._1 < 7) Math.min(xy._1, 4) else 6
    val scoreY = if (xy._2 < 7) Math.min(xy._2, 5) else 7
    val bonus = if (xy._1 > 3 && xy._2 > 4) 10 else 0
    -Math.abs(scoreX - scoreY) + 2*(scoreX + scoreY) + bonus
  }*/
  
  def toScore(xy: (Int, Int)): Int = {
    val (x, y) = xy
    val xScore = 
      if (x < 5 || y > 4) x 
      else 8 - x
    val yScore = 
      if (y < 6 || x > 3) y
      else 10 - y
    val bonusRect = if (x > 3 && y > 4) x + y else 0
    xScore + yScore - Math.abs(x - y)/2 + 1 + bonusRect
  }
  
  override def evaluate(b: Board) = {
    (b.own.toList map toScore).foldLeft(0.0){ _ + _ } 
  }
}