package qa.dsl

sealed trait State {
  def *(d: Double): State
  final def +(other: State): State = {
    CompositeState(this, other)
  }
}

object State {
  def apply(qubits: Int*): State = {
    SingularState(1.0, qubits.toVector)
  }
}

private[dsl] class SingularState private (val coefficient: Double, val qubits: Vector[Int]) extends State {
  override def *(d: Double): SingularState = {
    new SingularState(d * coefficient, qubits)
  }
}

object SingularState {
  def apply(coefficient: Double, qubits: Vector[Int]): SingularState = {
    val allowedQubitValues = Set(0, 1)
    require(qubits.forall(allowedQubitValues.contains))

    new SingularState(coefficient, qubits)
  }
}

private[dsl] class CompositeState private (val states: SingularState*) extends State {
  override def *(d: Double): CompositeState = ???
}

object CompositeState {
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
      new CompositeState(simplified.toSeq: _*)
  }

  private def flattenToSingularStates(states: List[State]): List[SingularState] = {
    states.flatMap{
      case ss: SingularState => Some(ss)
      case cs: CompositeState => cs.states
    }
  }

  private def sumCoefficients(states: List[SingularState]): Double = {
    states.map(_.coefficient).sum
  }

  private def backToSingularState(tt: (Vector[Int], Double)): SingularState = {
    SingularState(tt._2, tt._1)
  }
}
