package qa.dsl

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.matchers.{MatchResult, Matcher}

object ApproximateMatchers {
  val precision = 1e-6

  def approximatelyEquals(l: Double, r: Double): Boolean = (l - r).abs < precision
  def approximatelyEquals(l: SingularState, r: SingularState): Boolean =
    approximatelyEquals(l.coefficient, r.coefficient) && l.qubits == r.qubits

  def approximatelyEqual(right: State): StateApproximatelyEquals = new StateApproximatelyEquals(right)

  class StateApproximatelyEquals(right: State) extends Matcher[State] {
    override def apply(left: State): MatchResult = {
      val equals = (left, right) match {
        case (l: SingularState, r: SingularState) =>
          approximatelyEquals(l, r)
        case (l: CompositeState, r: CompositeState) =>
          l.states.size == r.states.size && l.states.forall{ lss =>
            r.states.exists(approximatelyEquals(_, lss))
          }
        case _ =>
          false
      }

      MatchResult(
        equals,
        s"$left did not (approximately) equal $right",
        s"$left did (approximately) equal $right"
      )
    }
  }
}

class ApproximateMatchersTest extends FlatSpec with Matchers {
  import ApproximateMatchers._

  "Two States with really small differences in their coefficients" should "be approximately equal" in {
    val something = 42 * State(0)
    val somethingSlightlyLarger = (42 + 0.5 * precision) * State(0)
    something should approximatelyEqual (somethingSlightlyLarger)
  }

  "Two States with considerable differences in their coefficients" should "not be approximately equal" in {
    val something = 42 * State(0)
    val somethingMuchLarger = 99 * State(0)
    something shouldNot approximatelyEqual (somethingMuchLarger)
  }

}
