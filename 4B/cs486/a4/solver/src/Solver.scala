package bayes

package object dsl {
  // ---- BEGIN DSL FOR MAKING PROBABILITIES PRETTY ----
  // Everything inside of this block can be safely skipped without changing
  // your understanding of the program. It's just boilerplate to make things
  // pretty


  // A Term is a collection of unique variables
  type Term = Set[Variable]

  // Interface for all variables
  trait Variable {
    def |(v: Term): Term = v + this
    def ∧(v: Variable): Term = Set(this, v)
    val not: Variable
    val canonical: Variable
  }

  // a "true" variable
  case class Var(label: String) extends Variable {
    val not = ¬(this)
    // canonical refers to a true version of the variable
    val canonical = this
  }

  // a "false" variable
  case class ¬(v: Var) extends Variable {
    val not = v

    val canonical = not
  }

  // Transforms a variable into a term
  implicit def toTerm(u: Variable): Term = Set(u)

  // Extend Terms to have the AND operator
  case class RichSetVar(u: Term) {
    def ∧(v: Variable): Term = u + v
  }
  implicit def toRichSet(u: Term): RichSetVar = RichSetVar(u)

  // A tuple representing the likelihood of a term
  type ProbCond = (Term, Double)

  // A factor is then a collection of probability conditions
  type Factor = Seq[ProbCond]

  // "Pimp my library" to add methods to a Factor (which is just a typedef)
  case class RichFactor(u: Factor) {
    // Does this factor contain a variable?
    def hasVariable(v: Variable) =
      u.exists(_._1.contains(v.canonical))

    def print =
      println(
        u .toSeq
          .sortBy(_._1.size)
          .mkString("\n") + "\n-\n")
  }
  implicit def toRichFactor(u: Factor): RichFactor = RichFactor(u)

  // Constructor for factors
  object Factor {
    def apply(probConds: ProbCond*): Factor = probConds.toSeq
  }

  // A little syntactic sugar so we can write P(...) -> 50%
  def P(c: Term) = c

  // ---- END DSL FOR MAKING PROBABILITIES PRETTY ----
  // Ok, here is where the real code starts



  def restrict(f: Factor, v: Variable) =
    f.filter(_._1.contains(v)) // keep only factors with this variable

  def product(f1: Factor, f2: Factor) = {
    for {
      (t1, p1) <- f1
      (t2, p2) <- f2
      // these tuples are now representative of elements of the cartesian
      // product: f1xf2

      // Check each variable in t1; see if its complement is in t2
      // if it is, discard this result from the product
      if !t1.exists(t2 contains _.not)
    } yield (t1 ++ t2, p1 * p2) // concat variables, multiply probabilities
  }

  def sum(f: Factor, v: Variable) = {
    // Collect terms by elimination of the variable and its negation
    f .groupBy(_._1 - v - v.not)
      .map { case (conds, group) =>
        // Fold each collection by the sum of its probabilities
        conds -> ((0.0 /: group)(_ + _._2))
      }
      .toSeq
  }

  def inference(
      factorList: Seq[Factor],
      q: Variable,
      evidence: Set[Variable],
      z: Seq[Variable]): Factor = {
    var F = factorList

    // z is the list of hidden variables to expand
    z.foreach { zj =>
      // Find factors containing this variable
      val fi = F.filter(_.hasVariable(zj))
      if (fi.length > 0) {
        val gj =
          sum(
            // product them all with one another
            fi.reduceLeft((f1, f2) => product(f1, f2)),
            zj) // then sum out variable zj

        gj.print

        // Add this new factor to the list
        F = F :+ gj
      }

      // Remove all factors containing zj from the list
      F = F.filterNot(_.hasVariable(zj))
    }

    // take the product of all remaining factors
    val beforeRestrict =
      F.reduceLeft((f1, f2) => product(f1, f2))

    // restrict by the evidence
    val unnormalized =
      (beforeRestrict /: evidence.toSeq)((f, v) => restrict(f, v))

    // calculate total probability mass
    val probMass = (0.0 /: unnormalized)(_ + _._2)
    restrict(
      unnormalized.map { case (c, p) =>
        // and then normalize the result
        c -> p / probMass
      },
      q) // restrict over the query value
  }
}

object Entrance {
  import dsl._

  def main(args: Array[String]): Unit = {
    val FS  = Var("FS")
    val FH  = Var("FH")
    val NH  = Var("NH")
    val FM  = Var("FM")
    val NA  = Var("NA")
    val FBF = Var("FBF")

    // Create the CPT. This is where all our DSL magic above goes!
    val fido =
      Seq(
        Factor(
          P(  FS                         ) -> 0.05,
          P(¬(FS)                        ) -> 0.95
          ),
        Factor(
          P(  FM                         ) -> 1/28.0,
          P(¬(FM)                        ) -> 0.96429
          ),
        Factor(
          P(  NA                         ) -> 0.30,
          P(¬(NA)                        ) -> 0.70
          ),
        Factor(
          P(  FBF |   FS                 ) -> 0.60,
          P(  FBF | ¬(FS)                ) -> 0.10,
          P(¬(FBF)|   FS                 ) -> 0.40,
          P(¬(FBF)| ¬(FS)                ) -> 0.90
          ),
        Factor(
          P(  NH  |   FM  ∧   NA         ) -> 0.80,
          P(  NH  |   FM  ∧ ¬(NA)        ) -> 0.40,
          P(  NH  | ¬(FM) ∧   NA         ) -> 0.50,
          P(  NH  | ¬(FM) ∧ ¬(NA)        ) -> 0.00,
          P(¬(NH) |   FM  ∧   NA         ) -> 0.20,
          P(¬(NH) |   FM  ∧ ¬(NA)        ) -> 0.60,
          P(¬(NH) | ¬(FM) ∧   NA         ) -> 0.50,
          P(¬(NH) | ¬(FM) ∧ ¬(NA)        ) -> 1.00
          ),
        Factor(
          P(  FH  |   FS  ∧   NH  ∧   FM ) -> 0.99,
          P(¬(FH) |   FS  ∧   NH  ∧   FM ) -> 0.01,
          P(  FH  |   FS  ∧   NH  ∧ ¬(FM)) -> 0.75,
          P(¬(FH) |   FS  ∧   NH  ∧ ¬(FM)) -> 0.25,
          P(  FH  |   FS  ∧ ¬(NH) ∧   FM ) -> 0.90,
          P(¬(FH) |   FS  ∧ ¬(NH) ∧   FM ) -> 0.10,
          P(  FH  |   FS  ∧ ¬(NH) ∧ ¬(FM)) -> 0.50,
          P(¬(FH) |   FS  ∧ ¬(NH) ∧ ¬(FM)) -> 0.50,
          P(  FH  | ¬(FS) ∧   NH  ∧   FM ) -> 0.65,
          P(¬(FH) | ¬(FS) ∧   NH  ∧   FM ) -> 0.35,
          P(  FH  | ¬(FS) ∧   NH  ∧ ¬(FM)) -> 0.20,
          P(¬(FH) | ¬(FS) ∧   NH  ∧ ¬(FM)) -> 0.80,
          P(  FH  | ¬(FS) ∧ ¬(NH) ∧   FM ) -> 0.40,
          P(¬(FH) | ¬(FS) ∧ ¬(NH) ∧   FM ) -> 0.60,
          P(  FH  | ¬(FS) ∧ ¬(NH) ∧ ¬(FM)) -> 0.00,
          P(¬(FH) | ¬(FS) ∧ ¬(NH) ∧ ¬(FM)) -> 1.00
        ))

    // a priori fido is howling
    inference(
      fido,
      FH,
      Set(),
      Seq(FBF, NA, NH, FM, FS)
    ).print

    println("\n\n==========\n\n")

    // fido sick given that he's howling and it's the full moon
    inference(
      fido,
      FS,
      Set(FM, FH),
      Seq(FBF, NA, NH)
    ).print
    println("\n\n==========\n\n")

    // ... and that he hasn't eaten
    inference(
      fido,
      FS,
      Set(FM, FH, FBF),
      Seq(NA, NH)
    ).print
    println("\n\n==========\n\n")

    // ... and that your neighbor is away
    inference(
      fido,
      FS,
      Set(FM, FH, NA, FBF),
      Seq(NH)
    ).print
  }
}
