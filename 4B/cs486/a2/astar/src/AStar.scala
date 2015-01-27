package tsp

import scala.collection.mutable.HashMap
import scala.collection.mutable.PriorityQueue

package object astar {
  // Solve is parameterized on the node type
  def solve[N](
      neighbors: N => Set[N], // function to get neighbors of a node
      cost: (N, N) => Double, // function to get the cost between two nodes
      heuristic: N => Double, // function to estimate distance to goal
      goal: N => Boolean,     // predicate to determine a goal
      start: N): Option[Seq[N]] = {

    var closed = Set[N]()

    // estimated total cost to goal
    val estimatedCost = new HashMap[N, Double]()

    // best distance to get to this node so far
    val bestCost = new HashMap[N, Double]()

    // Scala has weird priority queues; it requires an implicit Ordering[T]
    // to determine how to compare nodes. This creates one based on the
    // estimated total cost of a node.
    def orderProvider(node: N): Double =
      estimatedCost(node)
    implicit val nOrdering: Ordering[N]
      = Ordering.by(orderProvider).reverse

    var open = new PriorityQueue[N]()
    val cameFrom = new HashMap[N, N]()

    // Unfortunately, priority queues do not have a contains() method, so this
    // generates a predicate to determine if the node we are looking at is the
    // one we want
    def predicate(node: N): N => Boolean =
      n => n == node

    // Manages inserting a node into the open list; modifying its priority if
    // necessary.
    def insert(node: N, prio: Double) = {
      estimatedCost(node) = prio
      open += node
    }

    bestCost(start) = 0
    insert(start, 0)

    var popped = 0
    var pushed = 0
    while (!open.isEmpty) {
      val node = open.dequeue()
      popped += 1

      if (goal(node)) {
        println(popped + "/" + pushed)
        return Some {
          // Retrace the path used to get to node `from` recursively
          def backtrack(from: N): Seq[N] = {
            val before = cameFrom.get(from)
            if (before.isDefined)
              backtrack(before.get) :+ from
            else Seq(from)
          }

          backtrack(node)
        }
      }

      closed += node
      neighbors(node)
        .filterNot(closed contains _) // ignore nodes already closed
        .foreach { neighbor =>
          val totalCost = bestCost(node) + cost(node, neighbor)

          // If this node is not in the open list already, or the new cost is
          // better...
          if (open.count(predicate(neighbor)) == 0
              || totalCost < estimatedCost(neighbor)) {
            // Add this node to the priority queue
            cameFrom(neighbor) = node
            bestCost(neighbor) = totalCost
            pushed += 1

            val priority = totalCost + heuristic(neighbor)
            insert(neighbor, priority)
          }
        }
    }

    // Failed to find a path to the goal
    println("NO PATH")
    None
  }
}

