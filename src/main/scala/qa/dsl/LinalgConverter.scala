package qa.dsl

import qa.linalg.{UnitVector, Vector}
import scala.collection.immutable.{Vector => StdVector}

object LinalgConverter {

  val zeroElementRegardingTensorProduct = Vector(1)

  def toVector(s: State): Vector = {
    s match {
      case ss: SingularState => singularStateToVector(ss)
      case cs: CompositeState => compositeStateToVector(cs)
    }
  }

  def toState(v: Vector): State = {
    val numberOfQubits = lg2Unsafe(v.entries.size)

    val zeroState = StdVector.fill[Double](numberOfQubits)(0)

    ???
  }

  private def singularStateToVector(s: SingularState): Vector = {
    val tensoredUnitVectors = s.qubits.map(qubitToUnitVector).foldLeft(zeroElementRegardingTensorProduct)(_ ⊗ _)

    s.coefficient * tensoredUnitVectors
  }

  private def compositeStateToVector(s: CompositeState): Vector = {
    s.states.map(singularStateToVector).reduce(_ + _)
  }

  private def qubitToUnitVector(qubit: Int): Vector = {
    UnitVector(dimensions = 2, index = qubit)
  }

  private def lg2Unsafe(i: Int): Int = {
    (math.log(i) / math.log(2)).round.toInt
  }

}
