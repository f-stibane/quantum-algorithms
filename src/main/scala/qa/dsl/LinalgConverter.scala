package qa.dsl

import qa.linalg.{UnitVector, Vector}
import scala.collection.immutable.{Vector => StdVector}

object LinalgConverter {

  val zeroElementVector = Vector(1)
  val zeroElementState = SingularState(1, StdVector.empty)

  def toVector(s: State): Vector = {
    s match {
      case ss: SingularState => singularStateToVector(ss)
      case ss: SuperposedState => compositeStateToVector(ss)
    }
  }

  def toState(v: Vector): State = {
    val vectors = for {
      (d, i) <- v.entries.zipWithIndex
      if 0 != d
    } yield (d, UnitVector(dimensions = v.entries.size, index = i))

    val mappedToSingularQubitStates =
      vectors.map{ case (coeff, v) =>
        coeff * partializeVector(v).map(twoDimVectorToSingularState).foldLeft(zeroElementState)(_ ⊗ _)
      }

    SuperposedState(mappedToSingularQubitStates: _*)
  }

  // TODO: Make tail recursive - accumulated state as parameter
  private def partializeVector(v: Vector, alreadyFound: List[Vector] = List.empty): List[Vector] = {
    val lowerHalfsFirstIndex = v.entries.size / 2
    val indexOfNonZeroElement = v.entries.indexWhere(_ != 0)

    val singleQubitVector = if(indexOfNonZeroElement < lowerHalfsFirstIndex)
      Vector(1, 0)
    else
      Vector(0, 1)

    val remainingVector = if(indexOfNonZeroElement < lowerHalfsFirstIndex)
      v.entries.slice(0, lowerHalfsFirstIndex)
    else
      v.entries.slice(lowerHalfsFirstIndex, v.entries.size)

    if(remainingVector.size < 2)
      singleQubitVector :: alreadyFound
    else
      singleQubitVector :: partializeVector(Vector(remainingVector), alreadyFound)
  }

  private def twoDimVectorToSingularState(v: Vector): SingularState = {
    val indexOfNonZeroElement = v.entries.indexWhere(_ != 0)

    SingularState(coefficient = v.entries(indexOfNonZeroElement), qubits = StdVector(indexOfNonZeroElement))
  }

  private def singularStateToVector(s: SingularState): Vector = {
    val tensoredUnitVectors = s.qubits.map(qubitToUnitVector).foldLeft(zeroElementVector)(_ ⊗ _)

    s.coefficient * tensoredUnitVectors
  }

  private def compositeStateToVector(s: SuperposedState): Vector = {
    s.states.map(singularStateToVector).reduce(_ + _)
  }

  private def qubitToUnitVector(qubit: Int): Vector = {
    UnitVector(dimensions = 2, index = qubit)
  }

  private def lg2Rounded(i: Int): Int = {
    (math.log(i) / math.log(2)).round.toInt
  }

}
