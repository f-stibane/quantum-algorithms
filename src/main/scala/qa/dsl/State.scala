package qa.dsl

import scala.collection.immutable.{Vector => StdVector}
import scala.util.Random

sealed trait State {
  def measureSingleQubit(index: Int)(implicit random: Random = new Random()): (Int, State)

  def *(d: Double): State
  final def +(other: State): State = {
    SuperposedState(this, other)
  }
  def numberOfQubits: Int

  def ⊗(other: State): State = LinalgConverter.toState(LinalgConverter.toVector(this) ⊗ LinalgConverter.toVector(other))
  def inspectQubit(index: Int): State = ???
  def probability: Double
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

  override def numberOfQubits = qubits.length

  override def measureSingleQubit(index: Int)(implicit random: Random = new Random()): (Int, State) = {
    (qubits(index), this)
  }

  override def probability: Double = coefficient * coefficient
}

object SingularState {
  def apply(coefficient: Double, qubits: StdVector[Int]): SingularState = {
    val allowedQubitValues = Set(0, 1)
    require(qubits.forall(allowedQubitValues.contains))

    new SingularState(coefficient, qubits)
  }
}

private[dsl] class SuperposedState private(val states: SingularState*) extends State {
  override def *(d: Double): SuperposedState = new SuperposedState(states.map(_ * d): _*)

  override def toString: String = s"${states.mkString(" + ")}"

  override def numberOfQubits = states.head.numberOfQubits

  override def measureSingleQubit(index: Int)(implicit random: Random = new Random()): (Int, State) = {
    val groupedByQubit = states.groupBy(_.qubits(index))
    val prob0 = new SuperposedState(groupedByQubit(0): _*).probability
    val rolled = random.nextDouble()

    if(rolled < prob0) {
      (0, new SuperposedState(normalizeTo(groupedByQubit(0), probability): _*).reduced)
    } else {
      (1, new SuperposedState(normalizeTo(groupedByQubit(1), probability): _*).reduced)
    }
  }

  def normalizeTo(statesToNormalize: Seq[SingularState], toNormalizeTo: Double): Seq[SingularState] = {
    val originalProbability = probability(statesToNormalize)
    val factor = math.sqrt(toNormalizeTo / originalProbability)
    statesToNormalize.map(s => SingularState(s.coefficient * factor, s.qubits))
  }

  override def probability: Double = probability(states)
  private def probability(states: Seq[SingularState]): Double = states.map(_.probability).sum

  def reduced: State = {
    if(1 == states.size) states.head
    else this
  }
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
