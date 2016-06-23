package fundoku_two

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.collection.JavaConverters._

import cats._
import cats.syntax.cartesian._

case class Cell(value: Option[Int]) {
  if (value.isDefined && (value.get > 9 || value.get < 1) {
    throw new IllegalArgumentException("Value of cell is out of bounds.")
  }

  def toString(): String = value match {
    case Some(v) => v.toString
    case None => "-"
  }
}

object Cell {
  val empty = Cell(None)
}

case class Puzzle (cells: Seq[Cell]) {
  if (cells.count != 81) {
    throw new IllegalArgumentException("Number of cells is not equal to 81.")
  }

  def toString(): String = {
    ???
  }

  def isComplete(): Boolean = {
    ???
  }

  def copyAndUpdate(index: Int, value: Int): Puzzle = {
    this.copy(_.cells = cells.updated(index, Cell(Some(value))))
  }

  // Columns go vertically l -> r
  private def column(x: Int): Seq[Cell] = emptyIfOutOfBounds {
    cells.zipWithIndex.collect {
      case (c, i) if i % 9 == x => c
    }
  }

  // Rows go Horizonatally t -> b
  private def row(y: Int): Seq[Cell] = emptyIfOutOfBounds(y) {
    cells.grouped(9)(y)
  }

  private def rowAt(i: Int) = (i / 9) toInt

  private def colAt(i: Int) = i % 9

  // Box goes:
  //  0 | 1 | 2
  //  3 | 4 | ...
  private def box(i: Int): Seq[Cell] = emptyIfOutOfBounds {
    val rows = (0 to 2) map { _ + (i / 3).toInt } map row
    val cols = (0 to 2) map { _ % 9 } map column
    ???
  }

  // 0 indexed
  private def emptyIfOutOfBounds[A](x: Int)(action: () => A): A =
    if (x > 8 || x < 0) Seq()
    else action()

  private def isComplete(cells: Seq[Cell]) = completeCells == cells.map(_.value).toSet

}

object Puzzle {
  val empty = Puzzle(List.fill(81)(Cell.empty))

  private val completeCells = (1 to 9).map(Some(_)).toSet
}
