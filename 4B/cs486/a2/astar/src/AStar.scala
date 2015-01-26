import scala.collection.mutable.HashMap
import scala.collection.mutable.PriorityQueue

package object astar {
  // Solve is parameterized on the node type
  def solve[N](
      neighbors: N => Set[N], // function to get neighbors of a node
      cost: (N, N) => Int,    // function to get the cost between two nodes (guaranteed to be neighbors)
      heuristic: N => Int,    // function to estimate distance to goal
      goal: N => Boolean,     // predicate to determine a goal
      start: N): Option[Seq[N]] = {

    var closed = Set[N]()

    // estimated total cost to goal
    val estimatedCost = new HashMap[N, Int]()

    // best distance to get to this node so far
    val bestCost = new HashMap[N, Int]()

    // Scala has weird priority queues; it requires an implicit Ordering[T]
    // to determine how to compare nodes. This creates one based on the
    // estimated total cost of a node.
    def orderProvider(node: N): Int =
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
    def insert(node: N, prio: Int) = {
      // If the node is already in the queue, we need to remove it so we can
      // change its priority.
      if (open.count(predicate(node)) != 0)
        open = open.filterNot(predicate(node))

      estimatedCost(node) = prio
      open += node
    }

    bestCost(start) = 0
    insert(start, 0)

    while (!open.isEmpty) {
      val node = open.dequeue()

      if (goal(node))
        return Some {
          // Retrace the path used to get to node `from`
          def backtrack(from: N): Seq[N] = {
            val before = cameFrom.get(from)
            if (before.isDefined)
              backtrack(before.get) :+ from
            else Seq(from)
          }

          backtrack(node)
        }

      closed += node
      neighbors(node)
        .filterNot(closed contains _) // ignore nodes already closed
        .foreach { neighbor =>
          val totalCost = bestCost(node) + cost(node, neighbor)

          if (open.count(predicate(neighbor)) == 0
              || totalCost < estimatedCost(neighbor)) {
            cameFrom(neighbor) = node
            bestCost(neighbor) = totalCost

            insert(neighbor, totalCost + heuristic(neighbor))
          }
        }
    }

    // Failed to find a path to the goal
    None
  }
}

