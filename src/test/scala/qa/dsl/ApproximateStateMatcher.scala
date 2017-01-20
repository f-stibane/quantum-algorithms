package qa.dsl

import org.scalatest.matchers.{MatchResult, Matcher}
import qa.{ApproximateDoubleMatcher, Test}

object ApproximateStateMatcher {

  class StateApproximatelyEquals(right: State) extends Matcher[State] {
    override def apply(left: State): MatchResult = {
      val equals = (left, right) match {
        case (l: SingularState, r: SingularState) =>
          approximatelyEquals(l, r)
        case (l: SuperposedState, r: SuperposedState) =>
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

  def approximatelyEqual(right: State): StateApproximatelyEquals = new StateApproximatelyEquals(right)

  def approximatelyEquals(l: SingularState, r: SingularState): Boolean =
    ApproximateDoubleMatcher.approximatelyEquals(l.coefficient, r.coefficient) && l.qubits == r.qubits
}

class ApproximateStateMatcherTest extends Test {
  import ApproximateStateMatcher._

  "Two States with really small differences in their coefficients" should "be approximately equal" in {
    val something = 42 * State(0)
    val somethingSlightlyLarger = (42 + 0.5 * ApproximateDoubleMatcher.precision) * State(0)
    something should approximatelyEqual (somethingSlightlyLarger)
  }

  "Two States with considerable differences in their coefficients" should "not be approximately equal" in {
    val something = 42 * State(0)
    val somethingMuchLarger = 99 * State(0)
    something shouldNot approximatelyEqual (somethingMuchLarger)
  }

}
