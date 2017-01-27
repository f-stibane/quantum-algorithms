package qa.dsl

import scala.collection.immutable.{Vector => StdVector}

import qa.linalg.Matrix

/*
Very hacky implementation. Matrix gets built from knowledge when to apply the
operator (when all controlling qubits are 0).
Works, though.
TODO: Remove hacks
 */
trait ControlledOperator {
  import Operator.I

  def operator: Operator

  def apply(controlling: Int*)(controlled: Int)(s: State): State = {
    // TODO: requires

    val qubitsMatrix = matrix(controlling.toList, controlled)
    val involvedQubits = controlled :: controlling.toList

    var m = Matrix(1, 1, StdVector(1))
    for (_ <- 0 until involvedQubits.min) {
      m = m ⊗ I.matrix
    }
    m = m ⊗ qubitsMatrix
    for (_ <- (involvedQubits.max + 1) until s.numberOfQubits) {
      m = m ⊗ I.matrix
    }

    LinalgConverter.toState(m * LinalgConverter.toVector(s))
  }

  def matrix(controllingQubits: List[Int], controlledQubit: Int): Matrix = {
    val involvedQubits = controlledQubit :: controllingQubits
    val min = involvedQubits.min

    involvedQubitsMatrix(controllingQubits map { _ - min }, controlledQubit - min)
  }

  def involvedQubitsMatrix(controllingQubits: List[Int], controlledQubit: Int): Matrix = {
    val numberOfQubits = (controlledQubit :: controllingQubits).max + 1

    val matrixEntries = for (s <- allQubitStates(numberOfQubits)) yield {
      val state = State(s: _*)
      val stateWithOperatorApplied = if (shouldApplyOperator(s, controllingQubits)) {
        operator(controlledQubit)(state)
      } else {
        state
      }

      LinalgConverter.toVector(stateWithOperatorApplied)
    }

    Matrix(matrixEntries.length, matrixEntries.length, matrixEntries.map(_.entries).reduce(_ ++ _))
  }

  def allQubitStates(count: Int): List[List[Int]] = {
    (for (i <- 0 until math.pow(2, count).intValue) yield bitList(i, count)).toList
  }

  def bitList(value: Int, dim: Int): List[Int] = {
    var l = List.empty[Int]
    var remaining = value
    for (_ <- 0 until dim) {
      l = (if (0 == remaining % 2) 0 else 1) :: l
      remaining /= 2
    }
    l
  }

  def shouldApplyOperator(statesQubits: List[Int], controllingQubits: List[Int]): Boolean = {
    controllingQubits forall { 1 == statesQubits(_) }
  }
}

object ControlledOperator {
  object CNot extends ControlledOperator {
    override val operator = Operator.X
  }
}
