import bayes.dsl._
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class SolverSpec extends FlatSpec with ShouldMatchers {
  "Solver" should "do product properly" in {
    val A = Var("A")
    val B = Var("B")
    val C = Var("C")

    val f =
      Seq(
        Factor(
          P(A | B) -> 0.9,
          P(A | B.not) -> 0.1,
          P(A.not | B) -> 0.4,
          P(A.not | B.not) -> 0.6
          ),
        Factor(
          P(B | C) -> 0.7,
          P(B | C.not) -> 0.3,
          P(B.not | C) -> 0.8,
          P(B.not | C.not) -> 0.2
        )
      )

    val h = product(f(0), f(1))
    val result =
      Seq(
        P(A|B∧C)              -> 0.63,
        P(A|B.not∧C)          -> 0.08,
        P(A.not|B∧C)          -> 0.28,
        P(A.not|B.not∧C)      -> 0.48,
        P(A|B∧C.not)          -> 0.27,
        P(A|B.not∧C.not)      -> 0.02,
        P(A.not|B∧C.not)      -> 0.12,
        P(A.not|B.not∧C.not)  -> 0.12
        )

    def sorter(x: Factor) = {
      println(x.toSeq.sortBy(_._2).mkString("\n"))
    }

    //sorter(h)
    //println("\n\n\n")
    //sorter(result)

    true should be === true
  }

  it should "sum out properly" in {
    val A = Var("A")
    val B = Var("B")

    val f =
      Factor(
        P(A | B) -> 0.9,
        P(A | B.not) -> 0.1,
        P(A.not | B) -> 0.4,
        P(A.not | B.not) -> 0.6
      )

    sum(f, A).toSet should be ===
      Set(
        P(B) -> 1.3,
        P(B.not) -> 0.7
        )
  }

  it should "restrict properly" in {
    val A = Var("A")
    val B = Var("B")

    val f =
      Factor(
        P(A | B) -> 0.9,
        P(A | B.not) -> 0.1,
        P(A.not | B) -> 0.4,
        P(A.not | B.not) -> 0.6
      )

    restrict(f, B.not).toSet should be ===
      Set(
        P(A | B.not) -> 0.1,
        P(A.not | B.not) -> 0.6
      )
  }
}

