package tsp.sa

import scala.util.Random

case class Schedule(
    initTemp:      Double, // starting temperature
    minTemp:       Double, // ending temperature
    coolingFactor: Double, // rate the temperature cools
    itersPerTemp:  Int,    // how many iterations the temperature stays const
    isGeometric:   Boolean) {
  // Get a new temperature based on geometricity and cooling factor
  def nextTemp(oldTemp: Double): Double =
    if (isGeometric) oldTemp * coolingFactor
      else           oldTemp - coolingFactor
}

object SimulatedAnnealing {
  // Get a random element from an array. Used to pick a random neighbor.
  def randElem[T](array: Seq[T]): T =
    if (array.length == 1)
      // It's not random if there's only one option!
      array(0)
    else
      array(Random.nextInt(array.length - 1))

  def apply[N](
      neighbors: N => Seq[N], // neighbor generation function
      costFunc: N => Double,  // total cost of a node to be minimized
      schedule: Schedule,     // cooling schedule (see above)
      start: N): (N, Double) = {

    // Data structure to hold a solution
    case class Solution(answer: N) {
      val cost = costFunc(answer)
    }

    var soln = Solution(start)
    var best = soln

    var temp = schedule.initTemp

    // Predicate to determine whether or not to accept a worse solution `worse`
    def acceptWorse(worse: Solution): Boolean =
      math.exp((soln.cost - worse.cost) / temp) > Random.nextDouble

    var i = 0
    while (temp > schedule.minTemp) {
      val newSoln = Solution(randElem(neighbors(soln.answer)))

      if (newSoln.cost < soln.cost || acceptWorse(newSoln)) {
        soln = newSoln
        println(soln.answer)
      }

      if (soln.cost < best.cost)
        best = soln

      if (i % schedule.itersPerTemp == 0)
        temp = schedule.nextTemp(temp)

      i += 1
    }

    // Return the best answer and cost
    (best.answer, best.cost)
  }
}

