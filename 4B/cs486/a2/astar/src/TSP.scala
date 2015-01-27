package tsp

import scala.io.Source
import scala.util.Random
import tsp.astar._
import tsp.sa._


// Data structure to represent our cities
case class City(name: String, x: Int, y: Int) {
  // Hashmap of this node's neighbors to the cost of getting there
  val neighbors = new collection.mutable.HashMap[City, Double]()
}


object TSP {
  // Swap 2 elements in a list -- used to generate SA neighbors
  def swap2[T](list: Seq[T]): Seq[T] = {
    val elems =
      ( Random.nextInt(list.length - 1)
      , Random.nextInt(list.length - 1))

    val (elem1pos, elem2pos) =
      ( (math.min _).tupled(elems)
      , (math.max _).tupled(elems))

    val result = collection.mutable.MutableList(list: _*)
    result(elem1pos) = list(elem2pos)
    result(elem2pos) = list(elem1pos)
    result
  }

  // Euclidean distance between any two cities
  def cost(from: City, to: City): Double =
    math.sqrt(math.pow(from.x - to.x, 2) + math.pow(from.y - to.y, 2))



  // ------ SETUP THE TSP ENVIRONMENT -----
  def apply(source: Source) = {
    // Transform the source file into cities
    val cities =
      source.getLines.toList.tail.map { line =>
        val data = line.split(" ")

        City(data(0), data(1).toInt, data(2).toInt)
      }

    // Fill in the neighbors field for the cities, calculating costs along
    // the way
    cities.foreach { from =>
      cities
        .filter(_ != from) // do not add self to neighbors
        .foreach { to =>
          from.neighbors += to -> cost(from, to)
        }
    }



    // ----- A-STAR HEURISTIC HELPERS -----

    // Get the nearest city in `options` to `from`
    def nearestCity(from: City, options: Seq[City]): (City, Double) =
      from.neighbors.toList
        .filter(options contains _._1)
        .sortBy(_._2)
        .head

    // Build a minimal spanning tree from `cities` and compute the total
    // edge cost of it.
    def buildMST(cities: Seq[City]): Double = {
      val visited = collection.mutable.Set[City]()
      val edges =
        cities
          .flatMap(_.neighbors.toList)
          .sortBy(_._2) // _._2 is the cost
          .view         // make this lazy so the next filter is performed during folding
          .filterNot(visited contains _._1)

      // Fold over the edges, computing the total length. Edges are filtered in
      // realtime thanks to `edges` being lazy
      (0.0 /: edges) { case (acc, edge) =>
        visited += edge._1  // Add the node to the list of already visited nodes
        acc + edge._2       // result of fold is the sum over all edges matching
                            // the above predicate
      }
    }



    // ----- TSP-SPACE NODE -----

    // This class represents a partial tour
    case class Node(visited: Seq[City]) {
      // Current city this node is at
      val current = visited.last

      // Cities yet unvisited on this tour
      lazy val unvisited = cities.filterNot(visited contains _)

      override def toString(): String = {
        "Tour: " + visited.map(_.name).mkString("") + " (" + tourCost + ")"
      }

      // Compute the tour cost so far for this node
      lazy val tourCost =
        // Fold over the total cost so far, and the last node compared
        ((0.0, visited.head) /: visited.tail) { case ((acc, last), next) =>
          // The result of this fold is the cost from the last node to here,
          // plus the total cost before that. Additioanlly we need to track
          // the `next` node so we know where we are on the next iteration.
          (acc + last.neighbors(next), next)
        }._1 + (
          // If we have visited every city, add the extra cost of getting back
          // to the starting node.
          if (unvisited.length == 0)
            visited.last.neighbors(cities.head)
          else 0
        )

      // Compute the node's neighbors for A*
      lazy val astarNeighbors: Seq[Node] =
        unvisited.map { city =>
          // A neighbor is formed by adding an unvisited city to the current
          // tour.
          Node(visited :+ city)
        }

      // Compute simulated annealing neighbors.
      // Done by swapping two (non-starting) cities in the tour.
      def saNeighbors: Seq[Node] = {
        // Since this function generates a random neighbor, we only create
        // one for the SA procedure to use. It will get a different neighbor
        // each time it calls this function, however.
        val head = visited.head
        val tail = visited.tail
        Seq(
          // Never consider the head for swapping
          Node(head +: swap2(tail))
        )
      }

      // Get the cost of getting from this node to another node. Computed by
      // calculating the neighbor cost between the nodes' current cities.
      def costTo(node: Node): Double =
        current.neighbors(node.current) + (
          // If the next node is a complete tour, add the additional cost to get
          // back to the starting city.
          if (node.visited.length == cities.length)
            node.current.neighbors(cities.head)
          else 0
        )

      // A* heuristic to evaluate how unlikely this node is to be the best soln.
      def heuristic: Double = {
        if (isGoal)
          0
        else {
          // Compute a minimal spanning tree on the unvisited nodes -- this is
          // necessarily admissable.
          buildMST(unvisited)
        }
      }

      // This node is a goal if it has visited every city.
      val isGoal: Boolean =
        visited.length == cities.length
    }



    // ----- COMPUTE TSP -----

    val solution =
      SimulatedAnnealing[Node](
        _.saNeighbors,
        _.tourCost,
        Schedule(
          12,
          0.00001,
          0.93,
          250,
          true),
        Node(cities)
      )

    solution

    /*
    val solution =
      solve[Node](
        _.astarNeighbors.toSet,
        _ costTo _,
        _.heuristic,
        _.isGoal,
        Node(Seq(cities.head))
      ).get.last

    (solution.visited.map(_.name), solution.tourCost)*/
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println(
      (1 to 1).map { _ =>
        TSP(Source.fromFile("../randTSP/36/problem36"))
      }.sortBy(_._2).head)
  }
}

