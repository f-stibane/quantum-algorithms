package qa.dsl

import com.sun.xml.internal.xsom.XSWildcard.Other

import scala.collection.immutable.{Vector => StdVector}

sealed trait State {
  def *(d: Double): State
  final def +(other: State): State = {
    SuperposedState(this, other)
  }
}

object State {
  def apply(qubits: Int*): State = {
    SingularState(1.0, qubits.toVector)
  }
}

private[dsl] class SingularState private (val coefficient: Double, val qubits: StdVector[Int]) extends State {
  def ⊗(other: SingularState) = {
    SingularState(coefficient * other.coefficient, qubits ++ other.qubits)
  }

  override def *(d: Double): SingularState = {
    new SingularState(d * coefficient, qubits)
  }

  override def toString: String = {
    s"$coefficient|${qubits.mkString(",")}>"
  }
}

object SingularState {
  def apply(coefficient: Double, qubits: StdVector[Int]): SingularState = {
    val allowedQubitValues = Set(0, 1)
    require(qubits.forall(allowedQubitValues.contains))

    new SingularState(coefficient, qubits)
  }
}

private[dsl] class SuperposedState private(val states: SingularState*) extends State {
  override def *(d: Double): SuperposedState = ???

  override def toString: String = s"${states.mkString(" + ")}"
}

object SuperposedState {
  def apply(states: State*): State = {
    val singularStates = flattenToSingularStates(states.toList)
    require(singularStates.forall(_.qubits.size == singularStates.head.qubits.size))

    val simplified =
      singularStates
        .groupBy(_.qubits)
        .mapValues(sumCoefficients)
        .map(backToSingularState)

    if(simplified.size == 1)
      simplified.head
    else
      new SuperposedState(simplified.toSeq: _*)
  }

  private def flattenToSingularStates(states: List[State]): List[SingularState] = {
    states.flatMap{
      case ss: SingularState => Some(ss)
      case cs: SuperposedState => cs.states
    }
  }

  private def sumCoefficients(states: List[SingularState]): Double = {
    states.map(_.coefficient).sum
  }

  private def backToSingularState(tt: (StdVector[Int], Double)): SingularState = {
    SingularState(tt._2, tt._1)
  }
}
