package lt.vpranckaitis.math

import java.lang.Math._

object Math {
  type DistanceFunction = (Int, Int) => (Int, Int) => Double
  
  def manhattanDistance: DistanceFunction = {
     (x1, y1) => (x2, y2) => abs(x1 - x2) + abs(y1 - y2)
  }
  
  def chebyshevDistance: DistanceFunction = {
     (x1, y1) => (x2, y2) => max(abs(x1 - x2), abs(y1 - y2))
  }
  
  def euclideanDistance: DistanceFunction = {
     (x1, y1) => (x2, y2) => sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2))
  }
  
  
}