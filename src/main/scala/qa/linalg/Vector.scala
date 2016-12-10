package qa.linalg

import scala.collection.immutable.{Vector => StdVector}

case class Vector(entries: StdVector[Double]) {

  def *(d: Double) = Vector(entries.map(_ * d): _*)

  def ⊗(other: Vector): Vector = {
    val newVectorsEntries = for {
      a <- entries
      b <- other.entries
    } yield a * b

    new Vector(newVectorsEntries)
  }

}

object Vector {
  def apply(entries: Double*): Vector = {
    new Vector(entries.toVector)
  }
}

object UnitVector {
  def apply(dimensions: Int, index: Int): Vector = {
    val entries = List.fill[Double](dimensions)(0).updated(index, 1.0)
    Vector(entries: _*)
  }
}
