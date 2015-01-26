package astar

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class AStarSpec extends FlatSpec with ShouldMatchers {
  "AStar" should "find a path" in {

    val graph = new collection.mutable.HashMap[String, Set[(String, Int)]]()

    graph += "A" ->
      Set(
        ("B", 3),
        ("C", 5)
      )

    graph += "B" ->
      Set(
        ("C", 3),
        ("D", 5)
      )

    graph += "C" -> Set(("D", 1))
    graph += "D" -> Set(("E", 5))
    graph += "E" -> Set()

    solve[String](
      n          => graph(n).map(_._1),
      (from, to) => graph(from).filter(_._1 == to).head._2,
      _          => 0,
      _          == "E",
      "A"
    ) should be === Some(Seq("A", "C", "D", "E"))
  }
}
