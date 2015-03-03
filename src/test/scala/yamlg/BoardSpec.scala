package yamlg

import org.scalatest.FlatSpec
import lt.vpranckaitis.yamlg.game.Board

class BoardSpec extends FlatSpec {
  
  "Board" must "jumps right" in {
    val data = Board(flatten("""|00000000
                                |00000000
                                |00000000
                                |01112000
                                |00200000
                                |20000000
                                |00000000
                                |00000000"""))
    println(data.own)
    println
    println(data.other)
    println
    val expected = List(
      """|00000000
         |00000000
         |00000000
         |11012000
         |00200000
         |20000000
         |00000000
         |00000000""",
         
      """|00000000
         |00000000
         |00000000
         |01012000
         |00200000
         |20100000
         |00000000
         |00000000"""
    ) map { arrangement => Board(flatten(arrangement)) }
    
    assertResult(expected.toSet) {
      data.jump(2, 3).toSet
    }
    
  }
  
  def flatten(s: String) = {
    s.stripMargin filter { "012".indexOf(_) != -1 }
  }
}