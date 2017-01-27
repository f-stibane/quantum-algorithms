package qa.linalg

import scala.collection.immutable.{Vector => StdVector}

case class Matrix(rows: Int, cols: Int, entries: StdVector[Double]) {

  def *(v: Vector): Vector = {
    val multiplied = for {
      r <- 0 until rows
    } yield {
      val matrixFromIndex = r * cols
      val matrixToIndex = matrixFromIndex + cols

      val rowEntries = entries.slice(matrixFromIndex, matrixToIndex)

      rowEntries.zipWithIndex.map { case (entry, n) =>
        entry * v.entries(n)
      }.sum
    }

    Vector(multiplied.toVector)
  }

  def ⊗(other: Matrix): Matrix = {
    val tensored = for {
      r <- 0 until rows * other.rows
      c <- 0 until cols * other.cols
    } yield {
      val leftR = r / other.rows
      val leftC = c / other.cols
      val rightR = r % other.rows
      val rightC = c % other.cols

      entries(leftR * cols + leftC) * other.entries(rightR * other.cols + rightC)
    }

    Matrix(rows * other.rows, cols * other.cols, tensored.toVector)
  }

}
