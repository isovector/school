package tsp

import astar._
import scala.io.Source

object TSP {
  def apply(source: Source) = {
    case class City(name: String, x: Int, y: Int) {
      val neighbors = new collection.mutable.HashMap[City, Double]()
    }

    val cities =
      source.getLines.toList.tail.map { line =>
        val data = line.split(" ")

        City(data(0), data(1).toInt, data(2).toInt)
      }

    def cost(from: City, to: City): Double =
      math.sqrt(math.pow(from.x - to.x, 2) + math.pow(from.y - to.y, 2))

    cities.foreach { from =>
      cities.filter(_ != from).foreach { to =>
        from.neighbors += to -> cost(from, to)
      }
    }

    // --- TSP SOLVING BEGINS HERE ---

    def nearestCity(from: City, options: Seq[City]): (City, Double) =
      from.neighbors.toList
        .filter(options contains _._1)
        .sortBy(_._2)
        .head

    def buildMST(cities: Seq[City]): Double = {
      val visited = collection.mutable.Set[City]()
      val edges =
        cities
          .flatMap(_.neighbors.toList)
          .sortBy(_._2)
          .view
          .filterNot(visited contains _._1)

      (0.0 /: edges) { case (acc, edge) =>
        visited += edge._1
        acc + edge._2
      }
    }

    case class Node(visited: Seq[City]) {
      val current = visited.last
      lazy val unvisited = cities.filterNot(visited contains _)

      override def toString(): String = {
        "Tour: " + visited.map(_.name).mkString("") + " (" + tourCost + ")"
      }

      lazy val tourCost =
        ((0.0, visited.head) /: visited.tail) { case ((acc, last), next) =>
          (acc + last.neighbors(next), next)
        }._1 + (
          if (unvisited.length == 0)
            visited.last.neighbors(cities.head)
          else 0
        )

      lazy val neighbors: Seq[Node] =
        unvisited.map { city =>
          Node(visited :+ city)
        } ++ (
          if (visited.length == cities.length)
            Seq(Node(visited :+ visited.head))
          else
            Seq()
        )

      def costTo(node: Node): Double =
        current.neighbors(node.current) + (
          if (node.visited.length == cities.length)
            node.current.neighbors(cities.head)
          else 0
        )

      def heuristic: Double = {
        if (isGoal)
          0
        else {
          //unvisited.length
          //val toNext = nearestCity(current, unvisited)._2
          //val toHome = nearestCity(cities.head, unvisited)._2
          buildMST(unvisited)
         // toHome + toNext //+ mst
        }
      }

      def isGoal: Boolean =
        visited.length == cities.length
    }

    val solution =
      solve[Node](
        _.neighbors.toSet,
        _ costTo _,
        _.heuristic,
        _.isGoal,
        Node(Seq(cities.head))
      ).get.last
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    TSP(Source.stdin)
  }
}

