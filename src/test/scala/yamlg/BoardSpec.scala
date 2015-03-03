package yamlg

import org.scalatest.FlatSpec
import lt.vpranckaitis.yamlg.game.Board

class BoardSpec extends FlatSpec {
  
  "Board" must "jumps right" in {
    val zeros = (1 to (4 * 8)) map { _ => '0' } mkString ""
    val data = Board(flatten(s"""
      |00000000
      |01112000
      |00200000
      |20000000
      |$zeros"""))

    val expected = List(
        s"""
        |00000000
        |11012000
        |00200000
        |20000000
        |$zeros""",
        s"""
        |00000000
        |01012000
        |00200000
        |20100000
        |$zeros"""
    ) map { arrangement => Board(flatten(arrangement)) }
    
    assertResult(expected.toSet) {
      data.jump(2, 1).toSet
    }
    
  }
  
  it must "return empty sequence when there are nowhere to jump" in {
    val zeros = (1 to (6 * 8)) map { _ => '0' } mkString ""
    val data = Board(flatten(s"""
      |11100002
      |00000000
      |$zeros"""))
    
    assert(data.jump(0, 0).isEmpty)
    
  }
  
  it must "step right" in {
    val zeros = (1 to (4 * 8)) map { _ => '0' } mkString ""
    val data = Board(flatten(
        s"""
        |00000000
        |00112000
        |00200000
        |20000000
        |$zeros"""))

    val expected = List(
        s"""
        |00100000
        |00012000
        |00200000
        |20000000
        |$zeros""",
        s"""
        |00000000
        |01012000
        |00200000
        |20000000
        |$zeros"""
    ) map { arrangement => Board(flatten(arrangement)) }
    
    assertResult(expected.toSet) {
      data.step(2, 1).toSet
    }
    
  }
  
  def flatten(s: String) = {
    s.stripMargin filter { "012".indexOf(_) != -1 }
  }
}