package sudoku

import io.Source
import java.io.File

class Sudoku(board: Map[Int,Int]) {
  // An array of arrays of cells which are related to one another
  val constraints =
    (0 to 8).flatMap { i =>
      // Rows
      List((0 to 8).map { x =>
        i * 9 + x
      }) ++
      // Columns
      List((0 to 8).map { y =>
        i + y * 9
      })
    } ++
    (0 to 8).map { square =>
      // Squares
      val x = (square % 3) * 3
      val y = (square / 3) * 3 * 9

      (0 to 2).flatMap { dx =>
        (0 to 2).map(_ * 9).map { dy =>
          x + dx + y + dy
        }
      }
    }

  // Assigned cells => values
  val assignment = collection.mutable.HashMap[Int, Int]()

  // Possible values for a cell
  val possible = collection.mutable.HashMap[Int, Seq[Int]]()

  // Total number of assignments
  // set this to the negative ot the given positions, so that
  // once we are finished initializing it is 0
  var numAssigns = -board.toSeq.length

  (0 to 80).foreach { pos =>
    // When starting, every cell can have every value
    possible(pos) = (1 to 9)
  }

  board.foreach { case (pos, value) =>
    // Constrain the board by using the given values
    set(pos, value)
  }

  // Represents a mutation of the board that can be undone
  case class Undoable(pos: Int, value: Int, changes: Seq[Int]) {
    def undo() = {
      assignment -= pos

      // Re-add the possibilities
      changes.map { change =>
        possible(change) :+= value
      }
    }
  }

  // Try assigning pos to value
  def set(pos: Int, value: Int): Undoable = {
    assignment(pos) = value
    numAssigns += 1
    Undoable(pos, value, satisfyConstraints(pos))
  }

  // Helper function to indicate whether or not restricting this cell
  // requires anything to be undone
  def restrict(pos: Int, value: Int): Option[Int] = {
    // Check if this the value being restricted is still possible
    possible(pos).find(_ == value).map { v =>
      // If so, remove it from the possible values
      possible(pos) = possible(pos).filterNot(_ == v)
      pos
    }
  }

  // Restrict possible values of the board based on new position's value.
  // Returns a list of cells whose possibilities were reduced
  def satisfyConstraints(pos: Int): Seq[Int] = {
    val value = assignment(pos)

    constraints
      .filter(_ contains pos)   // get constraints which contain this position
      .flatMap(_.map { cell =>
        restrict(cell, value)}) // restrict any other cells in the constraint
      .flatten                  // transform Seq[Option[T]] into Seq[T]
      .distinct                 // only unique results
  }

  def solve(): Boolean = {
    // We've assigned the entire board
    if (assignment.toSeq.length == 81)
      return true

    val (restrictPos, values) =
      possible.toSeq
        .filterNot(assignment contains _._1)  // get unassigned cells
        .minBy(_._2.length)                   // find one with fewest options

    values.foreach { value =>
      // Try assigning each value
      val undo = set(restrictPos, value)

      if (solve())
        return true

      // Backtrack if this assignment didn't work
      undo.undo()
    }

    // Couldn't solve the board
    false
  }

  def getSolution(): Seq[Seq[Int]] = {
    // Get a linear arrray of our assignments
    val linear = assignment.toSeq.sortBy(_._1).map(_._2)

    // Split it into rows
    (0 to 8).map { i =>
      linear.slice(i * 9, (i + 1) * 9)
    }
  }

  def print = {
    println(getSolution.map(_.mkString(" ")).mkString("\n"))
  }
}

object Sudoku {
  def solve(filename: String): Int = {
    // Transform the file into an array of ints
    val file =
      Source
        .fromFile(filename)
        .getLines
        .mkString("") // join lines
        .split(" ")   // transform into an array
        .map(_.toInt) // mutate into ints

    // Transform array into map of assignments
    val board =
      // Loop over every cell
      (0 to 80).map { pos =>
        file(pos) match {
          case 0 => None            // nothing to do if it's 0
          case i => Some(pos -> i)  // we care about it otherwise
        }
      } .flatten  // get rid of nones
        .toMap

    // Solve the board and return the number of assignments
    val s = new Sudoku(board)
    s.solve()
    s.numAssigns
  }

  def main(args: Array[String]): Unit = {
    // Loop through the problems folder
    new File("problems")
      .listFiles
      .toList
      .map { size =>
        // Transform each into an (Int, Seq[Int])
        size.getName.toInt ->
          size.listFiles.toList
            // Solve each file
            .map(x => solve(x.getCanonicalPath))
      }
      .sortBy(_._1) // Sort by problem size
      .foreach { case (size, results) =>
        // Print average case
        println(size + ", " + (results.sum.toFloat / results.length))
      }
  }
}
