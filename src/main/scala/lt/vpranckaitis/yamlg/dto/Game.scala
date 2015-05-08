package lt.vpranckaitis.yamlg.dto

case class Game(gameId: Long, started: Int, winner: Int, boards: List[Board])