package lt.vpranckaitis

import scala.annotation.migration
import scala.collection.immutable.TreeMap
import scala.collection.immutable.BitSet
import scala.collection.immutable.HashSet
import scala.collection.mutable.TreeSet

object Benchmark {
  def timer(f : () => Any) = {
    val t1 = System.nanoTime()
    f.apply()
    val t2 = System.nanoTime()
    (t2 - t1) / 1000000.0
  }
  
  def benchmark(fs: Map[String, () => Any]) {
    val width = (fs.keys map { _.length }).max
    timer(() => Unit)
    val test = fs map { entry => (entry._1, timer(entry._2))}
    test foreach { entry => 
      val name = entry._1
      val t = entry._2
      println(String.format(s"%-${width}s : %10.6f", name.asInstanceOf[Object], t.asInstanceOf[Object]))
    }
  }
  
  def benchmarkTest {
    val t = TreeMap[String, () => Unit]("One" -> (() => Unit), "Twoooo" -> (() => Unit), "Three" -> (() => Unit))
    benchmark(t)
  }
  
  def benchmarkSets {
    val data = 1 to 1000000 by 17
    def createBitSet = { () => val s = BitSet(data: _*) }
    def createHashSet = { () => val s = HashSet(data: _*) }
    def createTreeSet = { () => val s = TreeSet(data: _*) }
    
    val bs = BitSet(data: _*)
    val hs = HashSet(data: _*)
    val ts = TreeSet(data: _*)
    
    def traverseBitSet = { () => bs foreach { _ => Unit } }
    def traverseHashSet = { () => hs foreach { _ => Unit } }
    def traverseTreeSet = { () => ts foreach { _ => Unit } }
    
    val search = 1 to 1000000 by 19
    def containsBitSet = { () => search foreach { bs.contains } }
    def containsHashSet = { () => search foreach { hs.contains } }
    def containsTreeSet = { () => search foreach { ts.contains } }
    
    val b1 = TreeMap("Create BitSet" -> createBitSet, "Create HashSet" -> createHashSet, "Create TreeSet" -> createTreeSet)
    val b2 = TreeMap("Traverse BitSet" -> traverseBitSet, "Traverse HashSet" -> traverseHashSet, "Traverse TreeSet" -> traverseTreeSet)
    val b3 = TreeMap("Contains BitSet" -> containsBitSet, "Contains HashSet" -> containsHashSet, "Contains TreeSet" -> containsTreeSet)
    
    benchmark(b1)
    println
    benchmark(b2)
    println
    benchmark(b3)
    println
  }
  
  def benchmarkLoops {
    val start = 1
    val end = 10000000
    val step = 7
    def forLoop = { () => for (i <- start until end by step) { } }
    def whileLoop = { () => 
      var i = start
      while (i < end) {
        i += step
      }
    }
    
    val b = TreeMap("For loop" -> forLoop, "While loop" -> whileLoop)
    benchmark(b)
    println
  }
}